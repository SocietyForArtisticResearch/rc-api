# What is this ?

This is a bunch of shared Elm types, JSON encoders and decoders that RCData and RCKeywords share.
These two projects are related: one side is running on the server (RCData) and the other side running in the client (RCKeywords), we want both programs to use the same data (RC expositions metadata, etc..). So both RCData and RCKeyword use this repository to encode and decode the RC objects (expositions, portals, keywords etc..).

At some point I plan to change all of these into Codecs. 

To build either RCData or RCKeywords, you need to put this repo in a sibling folder:

```
root 
	> rckeywords
	> rcdata
	> rcapi (this repo)
```

You will now be able to build both.

