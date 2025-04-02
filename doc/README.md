
# Overview

Totpot is a 2 factor Authentication Application that uses TOTP based authenication


## Pre-requistes

* Need a small path to Monster-UI to enable alternative MFA sources
* Need monster-UI app called TeaPot

## Installation

Create a doc in system_auth
```
PUT {{base_url}}/multi_factor
{
    "data": {
        "name": "Totpot Authenticator", 
        "id": "totp_auth",
        "enabled": true, 
        "provider_name": "totp", 
        "settings": {
        }
    }
}
```
## Enable TOTP authentication for account

```
POST {{base_url}}/accounts/{{account_id}}/security

{ "data": 
    { "auth_modules" :
        { "cb_user_auth": 
            { "multi_factor": 
                { "enabled": true, 
                  "configuration_id": "totp_auth", 
                  "account_id": "{{account_id}}"
                } 
            } 
        } 
    },
    "id": "configs_crossbar.auth"
}
```


