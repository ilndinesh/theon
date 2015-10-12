# Theon ![Version](https://img.shields.io/badge/latest-1.1.0.0-brightgreen.svg?style=flat-square)

A simple HTTP-to-Kafka relay built for speed

    $ theon --help
    theon - A simple HTTP-to-Kafka relay built for speed

    Usage: theon [-b|--broker HOST] [-p|--port PORT] (-c|--config FILE)

    Available options:
      -h,--help                Show this help text
      -v,--version             Show the application version
      -b,--broker HOST         Kafka broker address(es) (default: localhost:9092)
      -p,--port PORT           Port to serve application (default: 3000)
      -c,--config FILE         Path to configuration file (required)



## Configuration

Theon requires a JSON configuration file

    {
      // N.B. Each of the top-level keys are required, and comments like this
      // are allowed, so long as they're the only thing occupying the line

      // Kafka producer defaults
      // N.B. Check out the `librdkafka` docs for configuration details:
      // https://github.com/edenhill/librdkafka/blob/master/CONFIGURATION.md
      "kafka": {
        "message.send.max.retries": "5"
      },

      // Kafka topic defaults
      // N.B. Check out the `librdkafka` docs for configuration details:
      // https://github.com/edenhill/librdkafka/blob/master/CONFIGURATION.md
      "kafkaTopicDefaults": {
        "request.timeout.ms": "10000"
      },

      // Kafka topic overrides
      // N.B. Check out the `librdkafka` docs for configuration details:
      // https://github.com/edenhill/librdkafka/blob/master/CONFIGURATION.md
      "kafkaTopics": {
        "test.0": {
          "request.required.acks": "0"
        },
        "test.1": {
          "request.required.acks": "1"
        }
      },

      // Username-to-password mappings for HTTP basic auth
      // N.B. If this section is empty, basic auth will be disabled
      "basicAuth": {
        "theon": "greyjoy"
      }
    }




## HTTP API

### `GET /`

Returns the current version of Theon.

### `POST /:topic -d {}`

Request data will be relayed to the given `:topic` in Kafka. Multiple messages,
separated by newlines, will be sent as a batch.



## Changelog

### v1.1.1.0

- New `GET /` endpoint returns the application version

### v1.1.0.0

- Introduced config file for Kafka client and HTTP basic authentication

### v1.0.1.0

- Primitive support for HTTP basic authentication

### v1.0.0.0

- Initial implementation of the HTTP-to-Kafka relay
