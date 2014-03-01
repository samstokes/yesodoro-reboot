#!/bin/sh

if [ -n "$CLIENT_SESSION_KEY" ]; then
  base64 --decode <<<$CLIENT_SESSION_KEY >config/client_session_key.aes
fi

exec ./dist/build/yesodoro-reboot/yesodoro-reboot "$@"
