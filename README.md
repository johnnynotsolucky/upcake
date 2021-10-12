<h1 align="center">
  🧁 Upcake
</h1>

<p align="center">
  <em>Cupcakes for your API</em>
</p>

<p align="center">
  <a href="https://github.com/johnnynotsolucky/upcake/actions/workflows/build.yaml"><img src="https://github.com/johnnynotsolucky/upcake/actions/workflows/build.yaml/badge.svg" /></a>
</p>

## Overview

Upcake enables assertions to be performed against HTTP requests. Requests and
their assertions are defined in a YAML file and can have dependencies on each
other, provided that dependency requests are named. Assertions can be against
request timing data and any response data including headers, content and
response code.

The request URL, headers and content support template rendering with
[Handlebars](https://docs.rs/handlebars/4.1.3/handlebars/index.html) syntax.

Requests are run in parallel except where they have a dependency to another
request in which case they will wait until their dependencies have completed
before starting.

The response headers and content of a request is made available as part of the
template context for dependent requests.

## Installation

### With cargo

```bash
cargo install upcake
```

### From source

```bash
git clone https://github.com/johnnynotsolucky/upcake.git
cd upcake
cargo install --path .
```

## Usage

### Command-line options

- `-k`, `--insecure` - Allow insecure server connections when using SSL
- `-L`, `--location` - Follow redirects
- `-v`, `--verbose` - Verbose output
- `-s`, `--max-response-size` - Maximum response size in bytes
- `-e`, `--extra-vars` Set additional variables as key=value or YAML. To use a
  file, prepend the value with "@". Available in the template context in the `user` property.
- `--connect-timeout` - Maximum time allowed for connection
- `-c`, `--config-file` - Path to the request config file. Defaults to
  "Upcakefile.yaml". Required if `--url` is not set.
- `-u`, `--url` - Run default assertions against a URL. Will use the default
  request config. Required if `--config-file` is not set.
- `-X`, `--request-method` - Specify request method to use. Used in conjunction
  with `--url`. Defaults to "GET".
- `-H`, `--header` - Pass custom headers to server. Used in conjunction with
  `--url`.
- `--status-code` - Verify the response code. Used in conjunction with `--url`.
  Defaults to 200.

**Note:** Configuration set from the command line will override configuration for that
property set in the Upcakefile.

### Configuration

- `location` - Follow redirects. Overridden by `--insecure`.
- `insecure` - Allow insecure server connections when using SSL. Overridden by
  `--insecure`.
- `connect_timeout` - Maximum time allowed for connection. Overridden by
  `--connect-timeout`.
- `extra_vars` - Set additional variables from YAML mapping. Merged with vars
  set with `--extra-vars`. Available in the template context in the `user` property.
- `verbose` - Verbose output. Overridden by `--verbose`.
- `max_response_size` - Maximum response size in bytes. Overridden by
  `--max-response-size`.
- `requests` - List of request configurations.

### Request configuration

- `name` _optional_ -  Name of the request.
- `requires` _optional_ - List of named requests this request depends upon.
- `request_method` _optional_ - The HTTP method to use. Defaults to "GET".
- `data` _optional_ - Data to send with the request. Send the contents of a file by prefixing the value with an "@", for example "@path/to/body/template.hbs". Relative paths are relative to the directory of the loaded configuration file.
- `headers`_optional_ - Mapping of headers to be sent.
- `url`- The URL to make the request to.
- `assertions` _optional_ - A list of assertions to perform on the response. Defaults to a HTTP 200 assertion.

### Assertion configuration

- `type` - The type of assertion to use. See [available assertions](#available-assertions).
- `path` - The [jql](https://crates.io/crates/jql) path to the field the assertion should run against. Defaults to `.`. `path` is ignored on inner assertions, for example the [length](#length) assertion.
- `skip` _optional_ - Whether to skip the assertion. If set, requires a string value for the reason.

### Available assertions

In addition to the top-level assertion configuartion, each assertion has its own properties which are required to be set.

####  Between

Type: `between`

Assert that a value is within a range.

- `min` - Start of range.
- `max` - End of range.
- `inclusive` _optional_ - Whether to include `min` and `max`in the assertion.

##### Example

```yaml
- type: between
  path: ."response_code"
  min: 200
  max: 399
  inclusive: true
```

#### Equal

Type: `equal`

Assert that a value equals the given value.

- `value` - Value to assert.

##### Example

```yaml
- type: equal
  path: ."response_code"
  value: 200
```

#### Not Equal

Type: `not-equal`

Assert that a value is not equal to the given value.

- `value` - Value to assert.

##### Example

```yaml
- type: not-equal
  path: ."response_code"
  value: 204
```

#### Length

Type: `length`

Assert that the length of a value passes the given assertion

- `assertion` - Any assertion with the same configuration defined in [assertion configuration](#assertion-configuration).

##### Example

```yaml
- type: length
  path: ."headers".[]
  assertion:
    - type: equal
      value: 5
```

#### Contains

Type: `contains`

Assert that a response value contains the given value.

- For strings, it asserts that the substring is present in the value;
- For arrays, it asserts that the value is present in the array;
- For mappings (dictionary/object types), asserts that the input map is present in the response map.

- `value` - The value to assert is contained in the given value.

##### Examples

**Assert mapping contains all key/value pairs**

```yaml
- type: contains
  path: ."content".{}
  value:
    key: Value
    another_property: Some other value
```

**Assert array contains a value**

```yaml
- type: contains
  path: ."content"."my_integer_array".[]
  value: 10
```

or

```yaml
- type: contains
  path: ."content"."my_object_array".[]
  value:
    id: item_10
    value: Item Value
```

**Assert substring appears in response value**

```yaml
- type: contains
  path: ."content"."my_string"
  value: "value"
```

#### Exists

Type: `exists`

Assert that the given value exists as a key in the response value.

- `value` - The value to assert exists in the given mapping.

##### Example

```yaml
- type: exists
  path: ."content"."my_object".{}
  value: id
```

#### Greater than

Type: `greater-than`

Assert that a value is greater than the given value.

- `value` - Value to assert.

##### Example

```yaml
- type: greater-than
  path: ."response_code"
  value: 200
```

#### Greater than equal

Type: `greater-than-equal`

Assert that a value is greater or equal to the given value.

- `value` - Value to assert.

##### Example

```yaml
- type: greater-than-equal
  path: ."response_code"
  value: 200
```

#### Less than

Type: `less-than`

Assert that a value is less than the given value

- `value` - Value to assert.

##### Example

```yaml
- type: less-than
  path: ."response_code"
  value: 400
```

#### Less than equal

Type: `less-than-equal`

Assert that a value is less than or equal to the given value

- `value` - Value to assert.

##### Example

```yaml
- type: less-than-equal
  path: ."timing"."starttransfer"
  value: 100
```

### Response data

#### Request result

- `http_version` - The HTTP version used for the request.
- `response_code` - The HTTP response code returned.
- `response_message` - A HTTP response message returned from the host, if any.
- `headers` - A list of response headers.
- `timing` - Response timing data. See [timing results](#timing-results).
- `content` - Response content, either formatted as JSON, or raw content if it
couldn't be parsed as JSON.

#### Timing results

- `namelookup` - Duration in milliseconds from the start of the request until
name lookup resolved.
- `connect` - Duration in milliseconds from the start of the request until a
connection to the remote host is established.
- `pretransfer` - Duration in milliseconds from the start of the request until
file transfer was about to begin.
- `starttransfer` - Duration in milliseconds from the start of the request until
the first byte was received. AKA TTFB.
- `total` - Duration in milliseconds from the start of the request until the
request ended.
- `dns_resolution` - Alias for `namelookup`.
- `tcp_connection` - Difference of `connect` and `namelookup`.
- `tls_connection` - Difference of `pretransfer` and `connect`.
- `server_processing` - Difference of `starttransfer` and `pretransfer`.
- `content_transfer` - Difference of `total` and `starttransfer`.

## Examples

Examples are in [examples](examples/).

They are configured to run against a local [httpbin](https://httpbin.org/) server.

### httpbin Server

#### Start with docker

```bash
docker run -p 8888:80 kennethreitz/httpbin
```

#### Start with docker-compose

```bash
docker-compose --file examples/docker-compose.yaml up
```

### Run the examples

```bash
upcake --config-file examples/basic.yaml
```

### Command-line examples

#### Inline POST request

```bash
upcake --url http://localhost:8888/post -X POST
```

#### Inline request with custom header

```bash
upcake --url http://localhost:8888/get -H "X-My-Token: token"
```

#### Inline request with environment variable

```bash
MY_TOKEN=token upcake --url http://localhost:8888/get -H "X-My-Token: {{env.MY_TOKEN}}"
```

#### Validate inline request status code

```bash
upcake --url http://localhost:8888/post -X PATCH --status-code 405
```

#### Dependencies example with AUTH_TOKEN

```bash
AUTH_TOKEN=my_token upcake --config-file ./examples/pipeline.yaml
```

#### Validation failure

```bash
upcake --url http://localhost:8888/post -X PATCH
echo $?
```

#### Verbose output (libcurl)

```bash
upcake --url http://localhost:8888/get --verbose
```
