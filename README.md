# Theon

A simple HTTP-to-Kafka relay built for speed

```
$ theon --help
theon - A simple HTTP-to-Kafka relay built for speed

Usage: theon [-b|--broker HOST_PORT] [-p|--port NUM] [-a|--auth]
             [-u|--user USERNAME] [-w|--pass PASSWORD]

Available options:
  -h,--help                Show this help text
  -v,--version             Show the application version
  -b,--broker HOST_PORT    Kafka broker address(es) (default: localhost:9092)
  -p,--port NUM            Port to serve application (default: 3000)
  -a,--auth                Enable HTTP Basic Auth (default: false)
  -u,--user USERNAME       Username for HTTP Basic Auth (default: theon)
  -w,--pass PASSWORD       Password for HTTP Basic Auth (default: greyjoy)
```

## API

### `POST /:topic -d {}`

Request data will be relayed to the given `:topic` in Kafka. Multiple messages,
separated by newlines, will be sent as a batch.


## Changelog

### v1.0.1

- Primitive support for HTTP basic authentication

### v1.0.0

- Initial implementation of the HTTP-to-Kafka relay