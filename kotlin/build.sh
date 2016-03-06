#!/bin/bash

kotlinc -include-runtime tracer.kt math-java.kt main-java.kt -d tracer.jar
