# What is this ?

This is a bunch of shared types, encoders and decoders that RCData and RCKeywords share.
These two projects are related: one side is running on the server and the other side running in the client, we want both programs to use the same data (RC expositions metadata, etc..) . Initially, I just copied the files between, but then it gets confusing to make sure they are up to date, so better to just have one set of files for both and make this a mutual dependency.

To build either of these, you need to put this repo in a sibling folder:

```
root 
	> rckeywords
	> rcdata
	> rcapi (this repo)
```

You will now be able to build both.

