# aproxy.rb --- async proxy between an IRC server and a client
# Copyright (C) 1998-2005 Daiki Ueno

# Author: Daiki Ueno <ueno@unixuser.org>
# Created: 1998-09-28
# Keywords: IRC, riece

# This file is part of Riece.

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with GNU Emacs; see the file COPYING.  If not, write to the
# Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
# Boston, MA 02110-1301, USA.

require 'io/nonblock'
require 'socket'

class AProxy
  def initialize(host, port, size, back)
    @host = host
    @port = port
    @size = size
    @back = File.new(back, 'a') if back
  end

  def start
    $stdout.nonblock = true
    trap('STOP', 'IGNORE')
    trap('TSTP', 'IGNORE')
    socket = TCPSocket.new(@host, @port)
    $stdout.write("NOTICE CONNECTED #{$$}\r\n")
    wfds_in = []
    buf = ''
    loop do
      rfds, wfds, = select([socket, $stdin], wfds_in)
      unless wfds.empty?
        if buf.length > @size
          s = buf.slice!(0 ... @size)
          @back.write(s) if @back
        end
        begin
          until buf.empty?
            len = $stdout.syswrite(buf)
            buf.slice!(0 .. len)
          end
          wfds_in.clear
        rescue Errno::EAGAIN
        end
      end
      if rfds.include?(socket)
        line = socket.gets("\r\n")
        break unless line
        if line =~ /\A(?::\S+\s+)?PING\s+(.*)\r\n/i
          socket.write("PONG #{$1}\r\n")
        else
          wfds_in = [$stdout]
          buf << line
        end
      end
      if rfds.include?($stdin)
        line = $stdin.gets("\r\n")
        break unless line
        socket.write(line)
      end
    end
    socket.close
  end
end

if $0 == __FILE__
  require 'optparse'

  opt_size, opt_back = nil, nil
  opts = OptionParser.new do |opts|
    opts.banner = <<"End"
Usage: #{$0} [OPTIONS] host port
End
    opts.on('-s', '--size SIZE', 'Size of buffer.') do |size|
      opt_size = size.to_i
    end
    opts.on('-b', '--back BACK', 'Send outdated messages to BACK.') do |back|
      opt_back = back
    end
    opts.on_tail('--help', '-h', 'Show this message.') do
      $stdout.print(opts.to_s)
      exit(0)
    end
  end
  begin
    opts.parse!(ARGV)
  rescue OptionParser::ParseError
    $stderr.print(opts.to_s)
    exit(1)
  end

  AProxy.new(ARGV.shift, ARGV.shift, opt_size, opt_back).start
end
