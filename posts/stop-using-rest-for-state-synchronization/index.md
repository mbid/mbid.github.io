---
title: "Stop using REST for state synchronization"
date: "September 22, 2024"
lang: "en_US"
---

***tl;dr**: Most apps need state *synchronization*, not state *transfer*.
We should replace REST and friends with proper state synchronization protocols where appropriate.*

Apart from getting into van life in Europe and working on the [Eqlog](../type-checking-with-eqlog-parsing) Datalog engine, I've also spent some time of my sabbatical building various webapps.
The tech stack I used was React + Typescript for the frontend and a REST server implemented with Rust and the Axum library as backend.
Rust might be somewhat unusual, but I think otherwise this is a very typical setup.

What struck me is how incredibly cumbersome, repetitive and brittle this programming model is, and I think much of this is due to using REST as interface between the client and the server.
REST is a state *transfer* protocol, but we usually want to *synchronize* a piece of state between the client and the server.
This mismatch means that we usually implement ad-hoc state synchronization on top of REST, and it turns out that this is not entirely trivial and actually incredibly cumbersome to get right.

---

It's probably easiest to explain what I mean with an example that's part of most webapps in some variant:
An input element that allows the user to edit a piece of text that is saved to the backend database.
Using REST we can model this as a path, say `/api/foo`, that supports GET and POST methods to fetch or replace the text by a given value (perhaps also DELETE and PUT if you want to complicate things).
A React component that allows users to edit this piece of text will probably display a text input element, GET the initial value when the component is created and POST a new value when the text input loses focus.
We show an error message with a retry button if a request failed, and we display a spinner while requests are in-flight.
Here's what it might look like:
```typescript
function FooInput(): JSX.Element {
  // The value of the input element. Should be != null after we've fetched the
  // initial value.
  const [value, setValue] = useState<string|null>(null);

  const [showSpinner, setShowSpinner] = useState<boolean>(true);
  const [showError, setShowError] = useState<boolean>(false);

  useEffect(() => {
    (async () => {
      console.assert(showSpinner, 'showSpinner should be true initially');
      try {
        const response = await fetch('/api/foo');
        if (!response.ok) {
          throw new Error('Failed to fetch');
        }
        const data = await response.json();
        setValue(data);
      } catch (err) {
        setShowError(true);
      } finally {
        setShowSpinner(false);
      }
    })();
  }, []);

  async function postValue(): void {
    setShowSpinner(true);
    try {
      const response = await fetch('/api/foo', {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify(value),
      });
      if (!response.ok) {
        throw new Error('Failed to save');
      }
      setShowError(false);
    } catch (err) {
      setShowError(true);
    } finally {
      setShowSpinner(false);
    }
  }

  function handleChange(event: React.ChangeEvent<HTMLInputElement>): void {
    setValue(event.target.value);
  }

  return (
    <div>
      {value != null && <input type='text' value={value} onChange={handleChange} onBlur={postValue} />}
      {showSpinner && <div className='spinner'>Loading...</div>}
      {showError && (
        <div>
          An error occurred <button onClick={postValue}>Retry</button>
        </div>
      )}
    </div>
  );
}
```

Given that we just want to enable the user to edit one string in the database, there's quite a lot of boilerplate here.
Ideally, we'd just need to specify how to display the user interface and where to find the string in the database, but here we have to also deal with sending state back and forth, showing errors and displaying a spinner.
In a real app, you would admittedly put several controls in a single form, and you'd need a single set of this boilerplate for the whole form, not for each control.
But we're going to write similar but not quite the same code in several places since our app probably contains many more forms and the code above is difficult to abstract:
Other controls might have to GET and POST complex data types instead of just a string.
Or we might need several different endpoints to load and update data in our form.
And we might need to put spinner and error messages at different places relative to our controls depending on the specific component, and make sure that there's no weird jumping in the UI when we show or hide spinner and error message.

More importantly though, the code above is not even correct when there's more than one request in-flight at the same time.
We might get into this situation if the user changes the value twice in quick succession, say first to `"A"` and then to `"B"`.
This will fire off two POST requests with payload `"A"` and then payload `"B"` in this order.
Unfortunately, HTTP does not guarantee that the requests also arrive in this order at our server.
And even when the requests arrive in the same order that they were sent in, we're in trouble:
Since our server most likely handles multiple requests concurrently, it might still happen that the second request is processed before the first one.
This means that we might first save `"B"` to our database and then overwrite it with the older value `"A"` even though the user meant the final value to be `"B"`.
Our UI doesn't even indicate that something is not quite right and instead just shows "B" to the user, so this is quite bad.

One solution to this problem is to require the user to hit a "submit" button, which we disable while requests are in-flight, so that we cannot send the second request before we've received the response to the first request.
This is arguably bad UX though, as evidenced by most highly polished UIs (e.g. your browser's preference page) not requiring you to do this.
An alternative is to queue up requests so that we postpone the second request until we've received the first request, for example by wrapping or monkey-patching `fetch`.
The downside here is that we've now slowed down communication with the server.
Depending on how critical the app is, it might also be OK to just ignore the problem since it usually doesn't occur too often.

But even when our backend receives and processes the two requests in the same order that they were sent in, our logic for displaying the spinner is wrong:
Note that we set `showSpinner = true` before kicking of a POST request, and we set `showSpinner = false` when we receive the reply.
The problem is that when we set `showSpinner = false` at the end of a request, we don't take into account that other request might still be in-flight.
This results in us hiding the spinner when we receive the first response already even though the second request is still in progress.

We can fix the spinner logic by replacing the `showSpinner` flag by a `requestCount` integer, which we increment before and decrement after requests.
We then show the spinner whenever the count is greater than 0.

The final problem I want to mention, which most apps simply accept, is that if a user opens the app twice, then changes in one instance are not propagated to the second instance automatically.
We could get around this problem by replacing our initial GET request to fetch data with a subscription to server-sent events by which the server notifies the client about changes of the value.
This would require significant server-side work though.

---

As I wrote in the beginning, I think all of this incidental complexity arises because we're using a tool made for state *transfer* to solve a state *synchronization* problem.
So just replacing REST with other state transfer protocols, for example gRPC, won't help.

I'm not in a position to give an accurate overview of the field, but there are some initiatives to push for actual state synchronization mechanisms:
[Automerge](https://github.com/automerge/automerge), [Yjs](https://github.com/yjs/yjs), the [Braid working group](https://braid.org/), [Electric SQL](https://electric-sql.com/) and others.
Most of their approaches appear to be based on [CRDTs](https://en.wikipedia.org/wiki/Conflict-free_replicated_data_type).
Since I haven't seriously tested those tools, I can't say how mature they are at the moment.

Something that worries me about some of the CRDT work I've seen is that they seem to be optimizing for a situation where clients disconnect for extended periods ("local first").
But even for the very common situation where the involved parties are just a single client and the server over a reasonable internet connection (i.e., a normal webapp), so that state divergence occurs only for durations on the order of milliseconds or perhaps seconds, having a proper state synchronization mechanism would be incredibly useful.

In any case, I hope state synchronization technology will eventually mature to the point where it's common enough that I don't have to build bug-ridden ad-hoc state synchronization on top of REST over and over again.
