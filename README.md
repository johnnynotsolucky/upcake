<h1 align="center">
  🧁 Upcake
</h1>

<p align="center">
  <em>Cupcakes for your API</em>
</p>

<p align="center">
  <a href="https://github.com/johnnynotsolucky/upcake/actions/workflows/build.yaml"><img src="https://github.com/johnnynotsolucky/upcake/actions/workflows/build.yaml/badge.svg" /></a>
</p>

## (Provisional) Example config

```yaml
# Default request_method: "GET"
# Default assertions:
#   assertions:
#     - type: equal
#       path: ."response_code"
#       value: 204
- url: "https://httpbin.org/get?some_query_param={{env.MY_QUERY_PARAM}}"

- request_method: "POST"
  url: "https://httpbin.org/post"
  data: "@data.json"

- request_method: "GET"
  url: "https://httpbin.org/status/204"
  assertions:
    - type: equal
      path: ."response_code"
      value: 200

- request_method: "POST"
  # name: Test Httpbin.org POST
  url: "https://httpbin.org/post"
  headers:
    "Content-Type": "application/json"
    "Authorization": "Bearer {{env.AUTH_TOKEN}}"
  data: |
    {
      "some": "json",
      "data": "{{env.MY_ENV_VAR}}"
    }
  assertions:
    - type: between
      path: ."response_code"
      min: 200
      max: 399
      inclusive: true

    - type: equal
      path: ."response_code"
      value: 300
      skip: Fix this

    - type: not-equal
      path: ."response_code"
      value: 300

    - type: less-than
      path: ."timing"."dns_resolution_time"
      value: 100
      inclusive: true

    - type: contains
      path: ."headers".[]
      value:
        name: Content-Type
        value: application/json

    # httpbin.org response body
    - type: exists
      path: ."json"."headers".{}
      value: Accept

    - type: length
      path: ."headers".[]
      assertion:
        type: equal
        value: 7
```

## (Provisional) Example output

```bash
$ upcake ./cupcakes.yaml

TAP version 13
#
# GET https://httpbin.org/get
#
ok 1 - equals 200
#
# GET https://httpbin.org/status/204
#
not ok 2 - equals 200
  ---
  assertion: equals 200
  result: 204
  ---
#
# POST https://httpbin.org/post
#
ok 3 - between 200 and 399 (inclusive)
ok 4 - equals 300 # SKIP Fix this
ok 5 - not equals 300
ok 6 - less than 100 (inclusive)
ok 7 - contains {"name":"Content-Type","value":"application/json"}
ok 8 - exists Accept
ok 9 - length equals 7
1..9
```
