TCPTest
=======

TCP connection tester for VPN and not

At the moment provides a continous stream of informations about the
connection taking statistical data from the kernel via getsockopt with
the flag TCP_INFO.

More information can probably be read, in case send a pull request or a
ask in the issue tracker.

Options:
    -a --address       <address/ip>
    -p --port          <port>
    -i --interval      <report interval in ms>
    -t --send-interval <packet interval in ms>
    -s --size          <size of a packet>

