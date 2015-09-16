# Theon

A simple HTTP-to-Kafka relay built for speed

```
$ theon --help
theon - A simple HTTP-to-Kafka relay built for speed

Usage: theon [-b|--broker HOST_PORT] [-p|--port NUM]

Available options:
  -h,--help                Show this help text
  -v,--version             Show the application version
  -b,--broker HOST_PORT    Kafka broker address(es) (default: localhost:9092)
  -p,--port NUM            Port to serve application (default: 3000)
```

## API

### `POST /:topic -d {}`

Request data will be relayed to the give `:topic` in Kafka.