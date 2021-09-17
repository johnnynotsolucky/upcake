# Upcake

## (Provisional) Example config

```yaml
- request_method: "GET"
  url: "https://httpbin.org/get"
  assertions:
    - type: equal
      path: ."response_code"
      value: 200

- request_method: "GET"
  url: "https://httpbin.org/status/204"
  assertions:
    - type: equal
      path: ."response_code"
      value: 200

- request_method: "POST"
  # summary: Test Httpbin.org POST
  url: "https://httpbin.org/post"
  headers:
    - "Content-Type: application/json"
  data: '{"foo": "bar"}'
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
