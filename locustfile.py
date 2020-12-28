from locust import HttpUser, task, between
import random

class BaseUser(HttpUser):
    queries = [
        "mon",
        "new",
        "cam",
        "tor",
        "london",
        "richm",
        "van",
        "win"
    ]

    locations = [
       ("41.11121", "-74.06848"),
       ("38.73845", "-77.18498"),
       None
    ]

    @task
    def suggestions(self):
        q = random.choice(self.queries)
        l = random.choice(self.locations)
        if l:
            lat, lng = l
            self.client.get("/suggestions?q={0}&latitude={1}&longitude={2}".format(q, lat, lng))
        else:
            self.client.get("/suggestions?q={0}".format(q))

    wait_time = between(1, 5)
