#!/usr/bin/env bash

elm-make src/Main2.elm --output=app.js

node server.js
