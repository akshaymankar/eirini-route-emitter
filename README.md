# eirini-route-emitter

## How does this work

1. Collect the routes
  1. List all pods in a given namespace
  1. List all statefulsets in the namespace
  1. Use the owner references field on pods to find the owner statefulset
  1. Read routes from statefulsets. The statefulsets have annotation called `routes` on them, which is JSON representation of the routes which are supposed to route to the pod. It looks like:
     ```
     [
       {
         "hostname": "app.domain.example.com", //represents the hostname go-router listens to
         "port": 8080 //represents the port on the app
       },
       {
         "hostname": "second-hostname.domain.example.com", //represents the hostname go-router listens to
         "port": 9090 //a CF app can run different things on multiple ports
       },
     ]
     ```
   1. Create a route message for each route-pod combination, which looks like this in json:
      ```
      {
        "uris": ["app.domain.example.com"],
        "app": "name of pod",
        "private_instance_id": "name of pod",
        "host": "10.0.0.1", // IP Address of the Pod
        "port": 8080
      }
      ```
      **Note:** Multple routes with same port can be combined together by specifying each hostname in the uris array.
1. Publish the route messages as json on a given nats url with subject `router.register`
