## About Ubiquitous
Ubiquitous is a very easy-to-use library for persistent configuration storage. It automatically takes care of finding a suitable place to save your data, and provides simple functions to access and modify the data within.

## How To
Load ubiquitous through ASDF or Quicklisp.

    (ql:quickload :ubiquitous)

The main functions you will be using are `restore`, and `value`. The former loads the configuration according to a designator, and the latter is an accessor allowing you to retrieve and set properties.

    (restore 'my-config)
    (setf (value :test) "hi!")
    (value :test)

When values are set, it automatically saves the configuration to file. The location of the file is stored in `*storage-pathname*`, which is automatically computed according to what is most suitable for the given parameter and OS. On Windows it will be under `%HOME%\AppData\Local\common-lisp\ubiquitous\` and everywhere else under `~/.config/common-lisp/ubiquitous/`. The exact behaviour of the pathname choosing is documented in `designator-pathname`.

`value` doesn't take a single name, but rather a path to a configuration value. The way things are traversed is handled by the `field` generic accessor. It tries to handle a number of commonly used structures, but you might have to extend it for your own classes, if you want to store those directly and traverse them. If a place does not exist yet, Ubiquitous will try to augment it if possible by creating a hash-table. This allows you to directly write a long path without having to worry about the containers existing.

    (setf (value 'something 'that 'goes 6 'levels 'deep) "Calling from the depths!")

Often times for configuration one might want to specify a default value to use.

    (defaulted-value "localhost" :hostname)

In case you need to remove a value, there's `remvalue`.

    (remvalue 'something 'that 'goes 6 'levels 'deep)

By default, an extended s-expression format is used to store things in a file. If you need a different format, you can add methods to `read-storage` and `write-storage`, and set `*storage-type*` to your type name.

Ubiquitous in itself does not have any external dependencies, so you may also bundle it into a singular file to just `load` using ASDF:

    (asdf:operate :build-op :ubiquitous)

Which will produce an independent `ubiquitous.lisp` file in `(asdf:system-source-directory :ubiquitous)`.
