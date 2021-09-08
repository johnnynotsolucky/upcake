# Upcake

## (Provisional) Example config

```yaml
- request: "GET"
  url: "https://httpbin.org/get"
  connect_timeout: 10
  assertions:
    - type: equal
      path: ."response_code"
      value: 200

- request: "POST"
  url: "https://httpbin.org/post"
  assertions:
    - type: between
      path: ."response_code"
      min: 200
      max: 399
      inclusive: true

    - type: equal
      path: ."response_code"
      value: 200

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
        name: content-type
        value: application/json

    # httpbin.org response body
    - type: exists
      path: ."json"."headers".{}
      value: Accept
```
