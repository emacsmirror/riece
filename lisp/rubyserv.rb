# A simple IPC server for executing arbitrary Ruby program.
# The protocol is based on Assuan protocol of GnuPG.

require 'thread'
require 'stringio'

class RubyServ
  def initialize
    @buf = StringIO.new
    @que = Queue.new
  end

  def dispatch(line)
    case line.chomp
    when /\AD /
      return @buf << unescape($')
    when /\A(\S+)\s*/
      c = $1
      r = $'
      d = "dispatch_#{c.downcase}"
      if respond_to?(d, true)
        Thread.start do
          self.send(d, c, r)
        end
      else
        puts("ERR 103 Unknown command\r\n")
      end
    end
  end

  def dispatch_cancel(c, r)
    puts("ERR 100 Not implemented\r\n")
  end

  def dispatch_bye(c, r)
    puts("ERR 100 Not implemented\r\n")
  end

  def dispatch_auth(c, r)
    puts("ERR 100 Not implemented\r\n")
  end

  def dispatch_reset(c, r)
    puts("ERR 100 Not implemented\r\n")
  end

  def dispatch_end(c, r)
    enq_data
  end

  def dispatch_help(c, r)
    puts("ERR 100 Not implemented\r\n")
  end

  def dispatch_quit(c, r)
    puts("ERR 100 Not implemented\r\n")
  end

  def dispatch_eval(c, r)
    r = deq_data if r.empty?
    p r
    open('|-') do |f|
      if f
        d = f.read
        Process.wait
        send_data(d)
        if $?.success?
          puts("OK\r\n")
        else
          puts("ERR #{$?.exitstatus}\r\n")
        end
      else
        eval(r)
        exit
      end
    end
  end

  def escape(s)
    s.gsub(/[%\r\n]/) {|m| '%%%02X' % m[0]}
  end

  def unescape(s)
    s.gsub(/%([0-9A-Z][0-9A-Z])/, ['\1'].pack('H*'))
  end

  def send_data(d)
    begin
      r = [d.length, 998].min   # 998 = 1000 - CRLF
      (0 ... r).each do |i|
        r -= 2 if d[i] =~ /[%\r\n]/
      end
      puts("D #{escape(d[0 ... r])}\r\n")
      d = d[r .. -1]
    end until d.empty?
  end

  def enq_data
    @que.enq(@buf.string)
  end

  def deq_data
    @que.deq
  end
end

if $0 == __FILE__
  serv = RubyServ.new
  while gets
    serv.dispatch($_)
  end
end
