#!/bin/bash

set -e
set -x

kotlinc -include-runtime tracer.kt math-java.kt main-java.kt -d tracer.jar
kotlinc-js tracer.kt math-js.kt main-js.kt -output tracer.js
