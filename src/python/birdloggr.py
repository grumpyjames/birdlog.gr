from google.appengine.api import users
from google.appengine.ext import ndb

import calendar
from datetime import datetime
import json
import webapp2

class Greet(webapp2.RequestHandler):
    def get(self):
        user = users.get_current_user()
        if (user):
            self.response.headers['Content-Type'] = 'text/plain'
            self.response.write('Hello, %s' % user.nickname())
        else:
            self.response.status = 401

class Login(webapp2.RequestHandler):
    def get(self):
        user = users.get_current_user()
        if (user):
            self.redirect("/index.html")
        else:            
            self.redirect(users.create_login_url("/index.html"))

class Logout(webapp2.RequestHandler):
    def get(self):
        user = users.get_current_user()
        if (user):
            self.redirect(users.create_logout_url('/'))
        else:
            self.redirect("/index.html")

class Record(ndb.Model):
    species = ndb.StringProperty(required = True, indexed = False)
    count = ndb.IntegerProperty(required = True, indexed = False)
    location = ndb.GeoPtProperty(required = True, indexed = False)
    time = ndb.DateTimeProperty(required = True, indexed = False)
         
class Sequenced(ndb.Model):
    sequence = ndb.IntegerProperty(required = True, indexed = True)
    time = ndb.DateTimeProperty(required = True, auto_now_add = True, indexed = True)
    # present if replacement or deletion, absent if new
    replaces = ndb.IntegerProperty(required = False, indexed = False)
    # present if new or replacement, absent if delete
    record = ndb.StructuredProperty(Record, required = False, indexed = False)

def refersTo(typeObj):
    if (typeObj):
        return int(typeObj["refersTo"])
    else:
        return None

def rec(itemObj):
    if (itemObj):
        return Record(species = itemObj["species"],
                      count = int(itemObj["count"]),
                      time = datetime.utcfromtimestamp(float(itemObj["time"]) / 1000),
                      location = ndb.GeoPt(lat = float(itemObj["location"]["lat"]),
                                           lon = float(itemObj["location"]["lon"])))
                      

def record_key(user_id):
    return ndb.Key('Record', user_id)

def obj_or_nil(key, obj):
    if obj and key in obj:
        return obj[key]
    else:
        return None

def int_or_nil(key, obj):
    if obj and key in obj:
        return int(obj[key])
    else:
        return None

def toRecord(user, jsonObj):
    seq = int_or_nil("sequence", jsonObj)
    return Sequenced( parent = record_key(user.user_id()),
                      sequence = seq,
                      replaces = int_or_nil("sequence", obj_or_nil("refersTo", obj_or_nil("type", jsonObj))),
                      record = rec(obj_or_nil("item", jsonObj)))
               

def datastoreRepn(jsonRecords, user):
    return [toRecord(user, r) for r in jsonRecords]

@ndb.transactional
def commit(records):
    for r in records:
        r.put()
    return True

class Records(webapp2.RequestHandler):
    def post(self):
        user = users.get_current_user()
        if (user):
            uid = user.user_id()
            jobj = json.loads(self.request.body)
            commit(datastoreRepn(jobj, user))
            self.response.write("{}")
            self.response.status = 201
        else:
            self.response.status = 401

# jesus christ, python, wtf is wrong with you?
# this returns epoch millis in float form
# remember: all javascript numbers are IEE754 double precision.
def epoch_millis(dt):
    ts = float(calendar.timegm(dt.timetuple()))
    return ts + (dt.microsecond / 1000000.0)

def rec_to_obj(r):
    if (r is None):
        return None
    else:
        return {
            "species": r.species,
            "count": r.count,
            "location": {"lat": r.location.lat, "lon": r.location.lon},
            "time": epoch_millis(r.time)
        }

def type_from(sequenced):
    if sequenced.replaces is None:
        return {"name": "New"}
    elif sequenced.record is None:
        return {"name": "Delete", "refersTo": { "sequence" : sequenced.replaces }}
    else:
        return {"name": "Replace", "refersTo": { "sequence" : sequenced.replaces }}

def seq_to_obj(sequenced):
    return {
        "sequence": sequenced.sequence,
        "time": epoch_millis(sequenced.time),
        "type": type_from(sequenced),
        "record": rec_to_obj(sequenced.record)
    }

class Session(webapp2.RequestHandler):
    def get(self):
        user = users.get_current_user()
        if user:
            parent = record_key(user.user_id())
            recent = Sequenced.query(ancestor = parent).order(-Sequenced.sequence).fetch(25)
            lastSequence = recent[0].sequence if len(recent) > 0 else -1 
            self.response.status = 200
            
            self.response.write(
                json.dumps(
                    { "nick": user.nickname(),
                      "lastSequence": lastSequence,
                      "recent" : [seq_to_obj(o) for o in recent] }))       
        else:            
            self.response.status = 404
            
app = webapp2.WSGIApplication([
    ('/session', Session),
    ('/login', Login),
    ('/logout', Logout),
    ('/hello', Greet),
    ('/records', Records)
], debug=True)
