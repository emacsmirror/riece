# server.rb --- A simple IPC server executing Ruby programs.
# Copyright (C) 1998-2005 Daiki Ueno

# Author: Daiki Ueno <ueno@unixuser.org>
# Created: 1998-09-28
# Keywords: IRC, riece, Ruby

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

require 'thread'
require 'stringio'

class Server
  def initialize(outfile, errfile, logfile)
    @out = $stdout
    @err = $stderr
    $stdout = outfile ? File.new(outfile, 'a') : StringIO.new
    $stderr = errfile ? File.new(errfile, 'a') : StringIO.new
    @log = File.new(logfile, 'a') if logfile

    @buf = ''
    @que = Queue.new
    @thr = Hash.new
    @cnt = 0
  end

  def dispatch(line)
    @log.puts(line) if @log
    case line.chomp
    when /\AD /
      @buf << $'
    when /\A(\S+)\s*/
      c = $1
      r = $'
      d = "dispatch_#{c.downcase}"
      if respond_to?(d, true)
        Thread.start do
          self.send(d, c, r)
        end
      else
        send_line("ERR 103 Unknown command\r\n")
      end
    end
  end

  def dispatch_cancel(c, r)
    send_line("ERR 100 Not implemented\r\n")
  end

  def dispatch_bye(c, r)
    send_line("ERR 100 Not implemented\r\n")
  end

  def dispatch_auth(c, r)
    send_line("ERR 100 Not implemented\r\n")
  end

  def dispatch_reset(c, r)
    send_line("ERR 100 Not implemented\r\n")
  end

  def dispatch_end(c, r)
    enq_data
  end

  def dispatch_help(c, r)
    send_line("ERR 100 Not implemented\r\n")
  end

  def dispatch_quit(c, r)
    send_line("ERR 100 Not implemented\r\n")
  end

  def dispatch_eval(c, r)
    r = deq_data if r.empty?
    name = nil
    Thread.exclusive do
      while @thr.include?(name = @cnt.to_s)
	@cnt += 1
      end
      @thr[name] = Thread.current
    end
    send_line("S name #{name}\r\n")
    send_line("OK\r\n")
    Thread.current[:rubyserv_name] = name
    begin
      Thread.current[:rubyserv_error] = false
      Thread.current[:rubyserv_response] = eval(r, exec_env.empty_binding)
    rescue Exception => e
      Thread.current[:rubyserv_error] = true
      Thread.current[:rubyserv_response] =
        e.to_s.sub(/\A.*?\n#{Regexp.quote(__FILE__)}:\d+: /o, '')
    end
    send_line("# exit #{name}\r\n")
  end

  def dispatch_poll(c, r)
    thr = @thr[r]
    if !thr
      send_line("ERR 105 Parameter error: no such name \"#{r}\"\r\n")
    elsif thr.alive?
      send_line("S running #{r}\r\n")
      send_line("OK\r\n")
    else
      if thr[:rubyserv_error]
        send_line("S exited #{r}\r\n")
      else
        send_line("S finished #{r}\r\n")
      end
      if d = thr[:rubyserv_response]
        send_data(d.to_s)
      end
      send_line("OK\r\n")
    end
  end

  def dispatch_exit(c, r)
    thr = @thr[r]
    if !thr
      send_line("ERR 105 Parameter error: no such name \"#{r}\"\r\n")
      return
    end
    thr.kill if thr.alive?
    @thr.delete(r)
    send_line("OK\r\n")
  end

  def escape(s)
    s.gsub(/[%\r\n]/) {|m| '%%%02X' % m[0]}
  end

  def unescape(s)
    s.gsub(/%([0-9A-Z][0-9A-Z])/) {[$1].pack('H*')}
  end

  def send_data(d)
    d = escape(d)
    begin
      len = [d.length, 998].min   # 998 = 1000 - "D "
      send_line("D #{d[0 ... len]}\r\n")
      d = d[len .. -1]
    end until d.empty?
  end

  def enq_data
    d = unescape(@buf)
    @buf = ''
    @que.enq(d)
  end

  def deq_data
    @que.deq
  end

  def send_line(line)
    @out.puts(line)
    @log.puts(line) if @log
  end

  def exec_env
    env = Object.new
    def env.empty_binding
      binding
    end
    out, log = @out, @log
    env.instance_eval {@out, @log = out, log}
    def env.send_line(line)
      @out.puts(line)
      @log.puts(line) if @log
    end
    def env.output(s)
      send_line("# output #{Thread.current[:rubyserv_name]} #{s}\r\n")
    end
    env
  end
end

if $0 == __FILE__
  require 'optparse'

  opt_outfile, opt_errfile, opt_logfile = nil, nil, nil
  opts = OptionParser.new do |opts|
    opts.banner = <<"End"
Usage: #{$0} [OPTIONS]
End
    opts.on('-o', '--out OUTFILE', 'Send stdout to OUTFILE.') do |outfile|
      opt_outfile = outfile
    end
    opts.on('-e', '--err ERRFILE', 'Send stderr to ERRFILE.') do |errfile|
      opt_errfile = errfile
    end
    opts.on('-l', '--log LOGFILE', 'Send log to LOGFILE.') do |logfile|
      opt_logfile = logfile
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

  server = Server.new(opt_outfile, opt_errfile, opt_logfile)
  while gets
    server.dispatch($_)
  end
end
