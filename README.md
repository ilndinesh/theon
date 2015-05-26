# Theon

A simple HTTP-to-Kafka relay built for speed

```
$ theon --help
theon - A simple HTTP-to-Kafka relay built for speed

Usage: theon [-b|--broker HOST_PORT] [-t|--topic NAME] [-p|--port NUM]

Available options:
  -h,--help                Show this help text
  -v,--version             Show the application version
  -b,--broker HOST_PORT    Kafka broker (default: localhost:9092)
  -t,--topic NAME          Kafka topic name (default: theon)
  -p,--port NUM            Port to serve application (default: 3000)
```

## API

### `POST / -d {}`

Request data will be relayed to the configured topic in Kafka.