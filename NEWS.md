# shinyauthr 1.0.0

* New `loginServer` and `logoutServer` functions added. `login` and `logout` are now deprecated, full details in README.
* Tests added.
* CRAN submission

# shinyauthr 0.1.1

* Added cookie-based authentication

# shinyauthr 0.1.0

* Prepare for CRAN submission

# shinyauthr 0.0.99

* Switched to the sodium package for password hashing and decryption

If you plan to use hashed passwords with shinyauthr you now must use the [sodium package](https://github.com/jeroen/sodium) to hash your passwords and the `sodium_hashed = TRUE` argument of the `shinyauthr::login` module call for decryption to work appropriately.

This means that previously used `hashed` and `algo` arguments that interfaced with the `digest` package are now deprecated. If you had previously hashed your passwords with the digest package to use with shinyauthr, please re-hash them with `sodium` and use the `sodium_hashed` argument instead. 

Sorry for this breaking change, but sodium hashing provides added protection against brute-force attacks on stored passwords. More information on this [here](https://doc.libsodium.org/password_hashing/).
