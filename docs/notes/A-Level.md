# Novus
Client: tbc
Programming lanuage written in: GO (Then re-written in itself - both to be included for final submission)
IDE used: Visual Studio Code

### What is the project
A programming lanuage that can be configured to run on both an OS and dedicated hardware - (non OS specific functions, no built in functions on os / hardware specific libraries).

### What is the purpose
To have consistent syntax for a programming lanuage that can be ran on any hardware or on top of any operating system or in any embeded device.

### Client
IDK i'll find someone that wants it.

### Similar products
Lanuage such as C++ can run on both operating systems and dedicated hardware and micro controllers, however that is as the compiler has been re-written to support this. With my lanuage the compiler only has to be very slighty changed to support for systems and for dedicated hardware hasnt got to be changed at all assuming that the assembly functions are the same / similar. Even if the actual instructions are different, thats okay assiming that for example `mov` in one OS or CPU would still be `mov` on another CPU.

Another feature I have that other lanuages such as C++ have, is slightly faster compilation time, and even faster run time - as i've already made it, it also has tiny file size and the actuall compiler is also tiny so yeah, it's better lil bro.

### How i'm gonna solve this
Build a programming lanuage with no built in functions apart from assembly instructions such as mov, syscall and more that are needed. That means custom functions such as a print functon can deffined using the following.
```novus
fn print(msg: str) -> void {
    msg = msg + "\n";
    mov(x0, 1);
    mov(x1, msg);
    mov(x2, len(msg));
    mov(x16, 0x2000004);
    syscall();
}
```

This means that you could then just make you own print functions. It's basically assembly with extra features such as functions, variables, conditions, strings, loops, and more high level features. Some lanuages can already do this, however it's not it's primary feature and not portable between languages.

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