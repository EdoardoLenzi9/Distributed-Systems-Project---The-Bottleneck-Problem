#!/bin/bash
ip addr show | grep wlp || true
ifconfig | grep wlp || true