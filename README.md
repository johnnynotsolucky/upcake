# Upcake

## (Provisional) Example config

```yaml
request: "POST"
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

  - type: length
    path: ."json"."headers"."Accept"
    assertion:
      type: equal
      value: 3

  - type: contains
    path: ."json"."headers".{}
    value: Accept
```

