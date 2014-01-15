## Introduction

Erlang wrapper for xenserver api.

## Example

{% highlight erlang %}
1> {ok, Pid} = exapi:start_link("xen", "password").
2> exapi_vm:get_by_name_label(Pid, "test").
["OpaqueRef:f8e6019b-f1c1-5a3b-23ad-d87bf281deff"]
{% endhighlight %}
