This [Haskell](http://haskell.org)-based project converts a gitit-friendly markdown page to HTML for my WordPress blog.
It uses the project fix-symbols-gitit for tidying up symbols.

* Remove gitit page meta-data if present (before parsing)
* Remove "Introduction" section header
* Remove "`<!-- references -->`"
* Deepen all headers by two levels
* Remove comments surrounded by `<!--[ ... ]-->`
* Fix source links: "src/" --> "/blog/src/"
* Rewrite some code symbols (using `fix-symbols-gitit`)

