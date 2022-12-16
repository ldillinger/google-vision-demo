# Object Recognition Demo - Google Cloud Vision APIs

Follow along as we build an object-recognition service in Haskell, using the Google Cloud Vision API (https://console.cloud.google.com/apis/library/vision.googleapis.com).

This document will assume that you've already set up a Google Cloud account with the following:

- A Google Cloud project
- A service API key
- Enabled Cloud Vision API

At this point, you should be able to use Postman to submit a HTTP request with a base64-encoded image, and receive back a successful response. We're going to build a server that does this for you automatically, and saves the results to a local MYSQL table.

# Instructions

You will need to insert your Google API Key into the project manually, as I did not wish to expose my own key on github. A more thorough implementaiton would have provided some mechanism of feeding it into the project (command line args, environment variable, etc).

The endpoints are as described:

GET /images
GET /images/{imageId}
GET /images/?objects={csv-objects}
POST /images/
    {
        "image" : {
            "content" : "base-64-string-encoded content"
        },
        "label" : null,
        "detectObjects" : true
    }

POST with imageURI is not functional due to time constraints, see note on Google not respecting their own APIs

# Difficulties and Decisions

The language in the challenge document was imprecise due to brevity and open-endedness. This meant that assumptions had to be made about the intent.

## Data vs Metadata vs Filedata

In `POST /images`, it was stated:

> Returns a HTTP `200` OK with a JSON response body including the image **data**

While the other endpoints clearly state `metadata`, this endpoint states `data`, which might be interpreted to mean the image file data. However, it then goes on to list the contents that one would expect to include in the metadata, and it does not make much sense to return the file that we just sent. As such, we have interpreted this to mean that we return the same metadata as present in the rest of the endpoints.

## Identifier vs label vs imageId

It is implied but not outright stated that the persistant data image `identifier` is to be used as the `imageId` argument. This is sensible and interpreted to be the case. We have elected to use the hash


`Label` is less clear, and this is taken to mean a human-readable annotation with no requirement of uniqueness. Since it is optional, if not provided, `label` will take on the value of `imageId`.

## Generic vs hand deriving instances

Automatically deriving JSON instances are fine when you are producing and consuming using the same library, but this can often result in a verbose format. Due to the simplicity of the APIs, and the need to interact with a third-party API, we have opted to hand-write the necessary instances.

## Lenses

We have opted to not use lenses in this library. There are many to choose from, and I like lenses as optional packages. This library is small enough to not need them.

## Alternative prelude

I have used many of the alternative preludes. `rio` is especially nice, and I used it for several years, but I'm quite comfortable with the Haskell ecosystem and recently I've favored pulling in things explicitly to keep dependencies down. Admittedly, on this library, I've probably pulled in most of what `rio` would anyway.

## MYSQL linker issues

I saved the last day for adding the persistence layer - the server was otherwise complete, with the data being stored in-memory for the time being. I was just going to use mysql-simple because there are only a few queries and it's easy enough to write them out by hand.

However, I ran into a rather fatal snag when MACOS could not find my mysql installation.

    setup: The program 'mysql_config' is required but it could not be found

Then I remembered that I have a new M1 mac and needed to re-install some things for the first time 

    brew install mysql

Okay, but it needs pcre as well

    brew install pcre

And this is where everything halted. Despite about 4 hours of effort, I could not get cabal / ghc to find the pcre installation. For comparison, I've used this library on my older mac a few times with no problems.

    Resolving dependencies...
    Build profile: -w ghc-9.2.2 -O1
    In order, the following will be built (use -v for more details):
    - pcre-light-0.4.1.0 (lib) (requires build)
    - mysql-simple-0.4.8.1 (lib) (requires build)
    - demo-0.0.1 (lib) (first run)
    Starting     pcre-light-0.4.1.0 (lib)
    Building     pcre-light-0.4.1.0 (lib)

    Failed to build pcre-light-0.4.1.0.
    Build log ( /Users/leo/.cabal/logs/ghc-9.2.2/pcr-lght-0.4.1.0-0dc750b2.log ):
    Configuring library for pcre-light-0.4.1.0..
    Preprocessing library for pcre-light-0.4.1.0..
    compiling dist/build/Text/Regex/PCRE/Light/Base_hsc_make.c failed (exit code 1)
    rsp file was: "dist/build/Text/Regex/PCRE/Light/hsc2hscall1634-0.rsp"
    command was: /usr/bin/gcc -c dist/build/Text/Regex/PCRE/Light/Base_hsc_make.c -o dist/build/Text/Regex/PCRE/Light/Base_hsc_make.o --target=arm64-apple-darwin--lflag=--target=arm64-apple-darwin --target=arm64-apple-darwin --target=arm64-apple-darwin -D__GLASGOW_HASKELL__=902 -Ddarwin_BUILD_OS=1 -Daarch64_BUILD_ARCH=1 -Ddarwin_HOST_OS=1 -Daarch64_HOST_ARCH=1 -Idist/build/autogen -Idist/build/global-autogen -include dist/build/autogen/cabal_macros.h -I/Users/leo/.ghcup/ghc/9.2.2/lib/ghc-9.2.2/lib/../lib/aarch64-osx-ghc-9.2.2/bytestring-0.11.3.0/include -I/Users/leo/.ghcup/ghc/9.2.2/lib/ghc-9.2.2/lib/../lib/aarch64-osx-ghc-9.2.2/base-4.16.1.0/include -I/Users/leo/.ghcup/ghc/9.2.2/lib/ghc-9.2.2/lib/../lib/aarch64-osx-ghc-9.2.2/ghc-bignum-1.2/include -I/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/ffi -I/Users/leo/.ghcup/ghc/9.2.2/lib/ghc-9.2.2/lib/../lib/aarch64-osx-ghc-9.2.2/rts-1.0.2/include -I/Users/leo/.ghcup/ghc/9.2.2/include/
    error: Base.hsc:103:10: fatal error: 'pcre.h' file not found
    #include <pcre.h>
            ^~~~~~~~
    1 error generated.

    cabal: Failed to build pcre-light-0.4.1.0 (which is required by demo-0.0.1).
    See the build log above for details.

I verified that it was installed. I found the brew cellar when attempting `brew link pcre`

    /opt/homebrew/Cellar/pcre/8.45

No one likes mucking around with sourcing and linker issues, and 

Everything was there, so I figured I'd try adding to the cabal.project.local file

    extra-include-dirs:
        /opt/homebrew/Cellar/pcre/8.45/include/
    extra-lib-dirs:
        /opt/homebrew/Cellar/pcre/8.45/lib/
        

I tried the command-line flags

    cabal repl demo --extra-include-dirs=/opt/homebrew/Cellar/pcre/8.45/include/ --extra-lib-dirs=/opt/homebrew/Cellar/pcre/8.45/lib/

I tried the *.cabal file variants.

Re-reading the cabal documentation yields that I should have been pointing extra-lib-dirs at the directory containing the library, not the lib directory path within the library.

I tried

    cabal repl demo --extra-lib-dirs=/opt/homebrew/

And

    cabal repl demo --extra-lib-dirs=/opt/homebrew/lib --extra-include-dirs=/opt/homebrew/include

And

    cabal repl demo --extra-lib-dirs=/opt/homebrew/ --extra-include-dirs=/opt/homebrew/include

I tried updating to the latest GHC 9.4.2 (I've been on 9.2.4) with no effect, and I tried going back to 8.10.7, but could not, due to it requiring LLVM versions pre-dating and unsupported by the M1 macs.

I am faced to admit that this is likely an M1 mac problem, as I have encountered similar difficulties regarding C library FFI in several other libraries such as gloss.

So here, I must give up on mysql-simple.

My options are:

- Use raw mysql with completely manual queries
- Try and use nix and hope that I don't run into M1 mac trouble over there too
- Try and use persistent, which I have used before but it was some time ago, because I did a lot of GHCJS stuff and so don't do much TH.

Well, persistent + persistent-sqlite seems to work. We'll go with that. I've been meaning to use it as a backend store for another project anyway, so this is a good test run.

One thing I do like about persistent is that it does generate field names the way I like, eg:

    Foo
        bar String
        deriving Show

Yields

    data Foo = Foo
        { fooBar :: String
        }
        deriving stock (Show)

This means I can just Persist-ify my usual definitions without needing to change any call sites.

## SQL STRUCTURE

With more time, I'd break it up into an images table and have them reference a recognized objects table to allow for more complex querying, but because the above issue took up so much of our time, we're going to just go with a single flat table, and encode the recognized objects as a comma-separated value string.

## API structure

Due to the requested API structure, servant recognizes GetAllImages and SearchImages as the same endpoint. This begs the question of, is search a subset of get-all? It is feasible to want to separate these two endpoints if the search API is complicated or returns a different result type, but here it is simple enough that we can combine them into one API endpoint.

The alternative is breaking the proposed API contract a bit, and giving the search endpoint the `images/search` path.

We have opted to combine them for simplicity, and to match the challenge document as closely as possible. We simply knocked out the redundant API and forwarded it - see `forwardGetAllImagesHandler`. This allows us to split them apart easily in the future if it was needed.

## Error handling and Status Codes

There really isn't any. Or rather, we're relying on the inbuilt error handling of servant and persistent, and they really absorb a lot of the blow. In the limited amount of time I've had, being happy-path successful is acceptable considering that getting it working is the higher priority.

If I had more time I'd be more thorough and worry about things like sqlite table file permissions, returning 404 instead of erroring, and handling Google's cloud vision API a bit better.

## Fabricating a Sqlite LIKE filter

Surprisingly, the LIKE constraint is not present in persistent-sqlite. We expected this to be available, for searching our image objects CSV field, but had to implement it ourself. Had we a more proper database with a separate table for the list of recognized objects, this wouldn't be a problem as we could query with EQUALS, but this would also necessitated much more complex querying logic.

Note that we cannot use ILIKE (case-insensitive LIKE), as we get an error - this is likely due to differences in various sql-like implementations, and ILIKE may not be supported by sqlite. THis means that our search is case sensitive. We should squash to lowercase to solve this if we have time.

## Lack of limits

This server lacks things like SQL connection limits / pools, as well as limits / paging of results in the search-images and get-all-images endpoints.

## False positives

Searching for 'cat' will also return 'cattle' and 'caterpillar' and 'catfish'. This could be fixed by furter filtering of results after the sql query.

## Search - And vs or ambiguity

The following section does not specify whether to return objects matching dog AND cat, or dog OR cat.

    - GET `/images?objects="dog,cat"`
        - Returns a HTTP `200` OK with a JSON response body containing only images that have the detected objects specified in the query parameter.
        
It remains ambiguous because "containing only images that have the detected objects specified" can be read to mean anything from "containing only images that have (at least one of) the detected objects specified" or "containing only images that have (all of) the detected objects specified".

We have opted for the inclusive-or implementation, as it is the least restrictive, returning at least all of the results that you do want, and only some that you don't, if you wanted the conjunctive-and implementation. We feel that this it is a better assumption to allow false positives than to filter false negatives. We'd definitely clarify this behavior, and propose a search mechanism implementation that allows for both.

## Whether to store the images

The challenge doesn't actually state anywhere that the images are to be saved to the server. Rather, that an image may be supplied as a URL allows the actual image to bypass the server entirely, sending the url as an argument to the vision API directly.

## Lack of safety checks

This server makes no effort to protect against injections via the URL parameters. Images are not checked to be valid base64.

## Whether to update the label upon a second submission

We've opted to only update the objects field for previously encountered images. If a new label is supplied on subsequent submissions, it will be ignored.

## TLS

The last issue I dealt with before packing this up was an end-to-end test. I ran into some issues with TLS. The google servers require https. This was solved by using the tlsManager instead of the usual one. Initially this yielded the dreaded:

    ConnectionError (HttpExceptionRequest Request { ... }
    (InternalException (HandshakeFailed (Error_Packet_Parsing "Failed reading: invalid header type: 72\nFrom:\theader\n\n"))))

But then I remembered to change the port too

## Google not respecting their own APIs

After implementing the google APIs, testing revealed that they will throw an error if the user attempts to submit an image via URL, regardless of the API stating that it is supported. There is no time to investigate how to turn this on, as I suspect there are controls disabling this.

A simple fix is to download the image locally, base64 it, and send the request via content. However, I am stretching time too thin, and have opted to stop here.