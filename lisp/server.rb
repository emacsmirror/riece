# A simple IPC server executing Ruby programs.

require 'thread'
require 'stringio'

class Server
  def initialize
    @out = $stdout
    @err = $stderr
    $stdout = StringIO.new
    $stderr = StringIO.new

    @buf = ''
    @que = Queue.new
    @thr = Hash.new
    @cnt = 0
  end

  def dispatch(line)
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
        @out.puts("ERR 103 Unknown command\r\n")
      end
    end
  end

  def dispatch_cancel(c, r)
    @out.puts("ERR 100 Not implemented\r\n")
  end

  def dispatch_bye(c, r)
    @out.puts("ERR 100 Not implemented\r\n")
  end

  def dispatch_auth(c, r)
    @out.puts("ERR 100 Not implemented\r\n")
  end

  def dispatch_reset(c, r)
    @out.puts("ERR 100 Not implemented\r\n")
  end

  def dispatch_end(c, r)
    enq_data
  end

  def dispatch_help(c, r)
    @out.puts("ERR 100 Not implemented\r\n")
  end

  def dispatch_quit(c, r)
    @out.puts("ERR 100 Not implemented\r\n")
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
    @out.puts("S name #{name}\r\n")
    @out.puts("OK\r\n")
    Thread.current[:rubyserv_name] = name
    out = @out
    env = Module.new
    env.module_eval do
      @out = out

      def output(s)
	@out.puts("# output #{Thread.current[:rubyserv_name]} #{s}\r\n")
      end
      module_function :output
    end
    begin
      Thread.current[:rubyserv_error] = false
      Thread.current[:rubyserv_response] = eval(r, env.module_eval{binding()})
    rescue Exception => e
      Thread.current[:rubyserv_error] = true
      Thread.current[:rubyserv_response] = e.to_s.sub(/\A.*?\n/, '')
    end
    @out.puts("# exit #{name}\r\n")
  end

  def dispatch_poll(c, r)
    thr = @thr[r]
    if !thr
      @out.puts("ERR 105 Parameter error: no such name \"#{r}\"\r\n")
    elsif thr.alive?
      @out.puts("S running #{r}\r\n")
      @out.puts("OK\r\n")
    else
      if thr[:rubyserv_error]
        @out.puts("S exited #{r}\r\n")
      else
        @out.puts("S finished #{r}\r\n")
      end
      if d = thr[:rubyserv_response]
        send_data(d.to_s)
      end
      @out.puts("OK\r\n")
    end
  end

  def dispatch_exit(c, r)
    thr = @thr[r]
    if !thr
      @out.puts("ERR 105 Parameter error: no such name \"#{r}\"\r\n")
      return
    end
    thr.kill if thr.alive?
    @thr.delete(r)
    @out.puts("OK\r\n")
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
      @out.puts("D #{d[0 ... len]}\r\n")
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
end

if $0 == __FILE__
  server = Server.new
  while gets
    server.dispatch($_)
  end
end
