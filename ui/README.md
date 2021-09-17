# Glow UI

This browser-based user interface to Glow uses the Quasar.dev Javascript
framework.

## Installation

To install the required dependencies, run:

```sh
yarn install
```

## Running

To run the Glow UI, you will need two terminals. In one, run the
UI backend from `glow`:

```sh
glow start-server
```

In the other terminal, run (from the `glow/ui` directory):

```
quasar dev
```

This will start the front-end server and launch a browser.
You will get a big scary warning about a self-signed certificate;
this is normal and expected. Accept it to continue to the Glow UI!
