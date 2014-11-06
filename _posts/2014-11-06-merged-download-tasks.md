---
layout: post
title: Merged download tasks
---

I recently
[merged the `:download-messages` and `:download-flags` tasks][the-commit].
I wanted to do that in order to update our recorded modseq value more
consistently.

Earlier, BIC would update the modseq value after downloading flag
changes, using the highest modseq value it had seen in a FETCH
response.  However, if a mailbox had received new messages after the
latest flag change, its modseq value would have been higher than that
for the flag changes, and BIC would always think that the mailbox
needed syncing.

Now, BIC records the new modseq value after downloading both flags and
messages, using the value that the server reported upon SELECT.  My
reasoning is that we've gathered everything we're interested in, and
there might be messages that we don't care about according to the
download criteria, even if they have higher modseq values.

As a consequence, if the server supports the LIST-STATUS capability,
upon connection BIC will now only select mailboxes that have actually
changed to download changes.  For servers not supporting LIST-STATUS
(such as Gmail), the old behaviour remains: BIC will select every
mailbox to check its modseq value.  I'm considering whether I should
make it send a STATUS request for each mailbox on such servers, and
select only interesting mailboxes.

[the-commit]: https://github.com/legoscia/bic/commit/f2e3b65b0ffe1e83be15f05ae8a7146e13b9d8c1
