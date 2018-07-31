# Running the local node simulation

The reference behavior of the Emotiq chain with all nodes in the local
process. 

## Configuring

Only needs to be done once:

```lisp
(ql:quickload :emotiq/config/generate)
(emotiq/config/generate:ensure-defaults :force t :for-purely-local)
```    


## Running chain

After configuring

```lisp
(ql:quickload :emotiq/startup)
(emotiq:main)
```


