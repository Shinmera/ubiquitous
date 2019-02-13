## About Ubiquitous
Ubiquitous is a very easy-to-use library for persistent configuration storage. It automatically takes care of finding a suitable place to save your data, and provides simple functions to access and modify the data within.

## How To
Load ubiquitous through ASDF or Quicklisp.

    (ql:quickload :ubiquitous)

The main functions you will be using are `restore`, and `value`. The former loads the configuration according to a designator, and the latter is an accessor allowing you to retrieve and set properties.

    (restore 'my-config)
    (setf (value :test) "hi!")
    (value :test)

Ubiquitous will not perform any loading unless you tell it to. Thus, before the storage is truly persistent, you need to tell it where to `restore` from. Then, when values are set, it automatically saves the configuration to file. The location of the file is stored in `*storage-pathname*`, which is automatically computed according to what is most suitable for the given `restore` designator and OS. On Windows it will be under `%HOME%\AppData\Local\common-lisp\ubiquitous\` and everywhere else under `~/.config/common-lisp/ubiquitous/`. The exact behaviour of the pathname choosing is documented in `designator-pathname`.

`value` doesn't take a single name, but rather a path to a configuration value. The way things are traversed is handled by the `field` generic accessor. It tries to handle a number of commonly used structures, but you might have to extend it for your own classes, if you want to store those directly and traverse them. If a place does not exist yet, Ubiquitous will try to augment it if possible by creating a hash-table. This allows you to directly write a long path without having to worry about the containers existing.

    (setf (value 'something 'that 'goes 6 'levels 'deep) "Calling from the depths!")

Often times for configuration one might want to specify a default value to use.

    (defaulted-value "localhost" :hostname)

In case you need to remove a value, there's `remvalue`.

    (remvalue 'something 'that 'goes 6 'levels 'deep)

By default, an extended s-expression format is used to store things in a file. If you need a different format, you can add methods to `read-storage` and `write-storage`, and set `*storage-type*` to your type name. Since `(setf value)` automatically calls `offload` to persist the storage, this might lead to a lot of saving all over the place. In order to avoid this, you can bundle a block of operations in a `with-transaction` form, which will only perform an `offload` once the block exits.

Ubiquitous in itself does not have any external dependencies, so you may also bundle it into a singular file to just `load` using ASDF:

    (asdf:operate :build-op :ubiquitous)

Which will produce an independent `ubiquitous.lisp` file in `(asdf:system-source-directory :ubiquitous)`.

## Concurrency
By default Ubiquitous does not try to handle concurrent access in any way. The reason for this is not laziness, but merely the desire to avoid dependencies for those that don't need it. However, if you require safe concurrent access and handling of the storage, simply load `ubiquitous-concurrent` instead of `ubiquitous`. This will also pull in `bordeaux-threads` and establish additional methods around the standard definitions that will ensure concurrency safety.

This will still work irregardless of how many different storage objects you use, as the locking on the operations happens on the currently accessed storage object itself, rather than on a global lock. In order to avoid needless locking and unlocking, you should bundle your operations into a `with-transaction` block, which will only perform a lock once.

## Metadata
Since version 2.0, Ubiquitous will output a metadata header into the configuration file. It will typically have the following structure:

```
; meta (:version 1.0 :package "CL-USER")
```

The purpose of this header is to ensure print/read consistency. When `offload`ing, the current `*package*` is output into the header, so that when the configuration is `restore`d, unqualified symbols in the configuration are read with the same home package as they were printed with.

If the header is missing your configuration will still read fine, and since it is a comment, configurations will still be compatible when read with version 1.0 of Ubiquitous. An error is however signalled if the header is malformed, or the referred package cannot be found.

If you implement your own storage format, you should ensure that you output and respect the same header, or a similar header. See `maybe-read-metadata`, `with-processed-metadata`, `print-metadata`, and related functions.

## Shortcomings
A couple of shortcomings exist in Ubiquitous, almost by necessity. As you might know out of experience, certain modifying operations are not possible to do without being able to modify the container of the object itself. As an example, `pop`ing an element off the head of the list requires setting the variable that contains the list, rather than the list itself. This sort of thing is rather annoying to model in a generic manner without complicating the common case needlessly. Furthermore, in a couple of instances ambiguity arises due to multiple actions being possible.

In detail, the following operations are supported suboptimally, or not at all:

* `remfield` on a `list` with one element left that is to be removed. The `car` of the cons will simply be set to `NIL`.
* `remfield` on a `vector`. This would require shifting fields and potentially adjusting it. The desired effect cannot be estimated and so Ubiquitous does not support the operation at all.
* `remfield` on a `standard-object`, as class-slots cannot be removed without the MOP and you probably wouldn't want that to happen anyway.
* `setf` `field` on a `list` or `vector` where the field is an index exceeding the length. In this case, an error is signalled as extending the object to the required length might not be a desired or possible effect.
* `setf` `field` on a `list` where the field is a symbol or string. Lists are commonly used as alists or plists. Ubiquitous strictly expects alists.

Another shortcoming is in the department of serialisation. Ubiquitous does not try to be overly smart about things, which especially comes into effect when serialising `standard-object`s. Ubiquitous saves the class' slots and restores it by calling `allocate-instance` without initargs and then `setf slot-value`-ing one slot after the other. If you need more tailored support for serialising your object, you must extend `define-ubiquitous-reader` and `define-ubiquitous-writer`, or write a new storage format altogether. Furthermore, since the default behaviour is to use the lisp printer and reader (with special handling for `hash-table`, `standard-object`, `standard-class`, and `package`) to serialise objects, several things might get lost in translation, such as the fill-pointer and adjustability of a vector.
