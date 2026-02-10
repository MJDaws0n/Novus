# Novus
Client: tbc
Programming lanuage written in: GO (Then re-written in itself - both to be included for final submission)
IDE used: Visual Studio Code

## Base plan
- Write a basic plan to fully understand how programming languages are made.
[Steps to building a lanugage](/steps.md).

- Write specific instructions for each such as parser.md, lexer.md etc..

- Make an `example.nov` file to work out exactly how I and the client want the language to look.

- Learn basics of go, then write a basic [main.go](/cmd/novus/main.go) file that just ouputs the version and syntax of the app.

- Check with the client that they are happy with the syntax of the lanuage.

- Make any adjustments they require.

- Make base files `lexer`, `parser`, `semantic analysis`, `ast`, `code gen`.

- Spend a few days making each, write test as I go along making each so we are not just releying on the `example.nov` file.

- Once fully built test fully on different operating systems and many different codes.

- Get people to test by writting samples of code to try and deduct an errors that may be with the lanuage.

- Build some basic built in packages as the way the client requested the lanuage to be built is made to be as lightweight as possible and cannot do much on it's own. I like the name `essentials` but might work on it and see what the client thinks about it. Each operating system will most likely need it's own package, but once basic functions for mac, windows and linux have been made it should be as simple as making some more ease of use built in functions such as regex, string contains, suff like that basically.

- Check in that the client is completly happy with everything.

- Fully test with different CPU's on different operating systems.

- If everything looks good, implement arrays and objects as that's something I think would be useful, the client said it's not required, however it deffinitely could be useful.