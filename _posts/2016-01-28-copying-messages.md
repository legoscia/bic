---
layout: post
title: Copying messages
---

I want to implement copying messages from one mailbox to another.  Why
not go straight for moving, you might ask — I decided to do it this
way because the `COPY` command is defined in RFC 3501, the basic IMAP
RFC, and thus pretty much guaranteed to be present in every server,
while the `MOVE` command is defined as an extension in RFC 6851.
Thus, the client is likely to come across servers that don't support
the `MOVE` command, and then it will have to emulate moving by first
copying the message, and then deleting it in the original mailbox.  So
`COPY` is the simpler operation.

Of course, since this is BIC, copying needs to work offline as well as
online.  I figured a good way to do this could be to extend the
`pending-flags` file, that currently keeps data about flag changes
that haven't been pushed to the server yet.  It has one line per
change, consisting of the uidvalidity value, the message UID, a `+` or
`-` marker, and then the name of the flag that's being added or
removed.  A natural extension might be using `C` as the "copy marker",
and then put the name of the target mailbox.

That's the easy part.  After recording the copy action, in the
`pending-flags` file, we'll have to actually perform the copy.  The
`bic--apply-pending-flags` function currently searches through that
file for lines with `+` or `-` markers, tries to optimise flag changes
into fewer commands, and then checks the results.  If all command
results were positive, it deletes the relevant region of the
`pending-flags` file.

So I'll have to change that function to include the `COPY` commands in
that dance.  And it would be nice to improve the behaviour on errors:
currently, if there's one flag that the server doesn't accept, the
entire batch of flag changes is kept in the file and retried every
time we connect to the server.  For copying, we have another bunch of
possible error conditions, such as target mailbox not existing, disk
full, etc.  And this function has become a bit of a sprawling mess.
Hopefully I can use this opportunity to make things better…
