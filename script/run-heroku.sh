#!/bin/bash

# Allows us to set the client session secret via a Heroku config variable
if [ -n "$CLIENT_SESSION_KEY" ]; then
  # Yesod's Web.ClientSession module already knows how to read the secret from a
  # file, so we just write to the file configured in Foundation.hs (in
  # makeSessionBackend)
  base64 --decode <<<$CLIENT_SESSION_KEY >config/client_session_key.aes
fi

exec yesodoro-reboot "$@"
