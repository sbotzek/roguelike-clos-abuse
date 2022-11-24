# Development Log

# 2022-11-22
Project started.  I wrote a very basic game loop and input parser.

The way I did the input parser isn't great.  And I'm pretty sure interning user input is a bad idea.  But, oh well.  It isn't forever.

## Version 2
At the very least, I should support listing commands.  And I don't know if it's possible with how I did things before.

So I switched it to just be a list of input handlers rather than relying on CLOS.
