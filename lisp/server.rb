# A simple IPC server executing Ruby programs.

require 'thread'

class Server
  module B
    def output(s)
      puts("# output #{Thread.current[:rubyserv_name]} #{s}\r\n")
    end
    module_function :output
  end

  def initialize
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
    name = nil
    Thread.exclusive do
      while @thr.include?(name = @cnt.to_s)
	@cnt += 1
      end
      @thr[name] = Thread.current
    end
    puts("S name #{name}\r\n")
    puts("OK\r\n")
    Thread.current[:rubyserv_name] = name
    begin
      Thread.current[:rubyserv_error] = false
      Thread.current[:rubyserv_response] = eval(r, B.module_eval('binding()'))
    rescue Exception => e
      Thread.current[:rubyserv_error] = true
      Thread.current[:rubyserv_response] = e
    end
    puts("# exit #{name}\r\n")
  end

  def dispatch_poll(c, r)
    thr = @thr[r]
    if !thr
      puts("ERR 105 Parameter error: no such name \"#{r}\"\r\n")
    elsif thr.alive?
      puts("S running #{r}\r\n")
      puts("OK\r\n")
    else
      if thr[:rubyserv_error]
        puts("S exited #{r}\r\n")
      else
        puts("S finished #{r}\r\n")
      end
      if d = thr[:rubyserv_response]
        send_data(d.to_s)
      end
      puts("OK\r\n")
    end
  end

  def dispatch_exit(c, r)
    thr = @thr[r]
    if !thr
      puts("ERR 105 Parameter error: no such name \"#{r}\"\r\n")
      return
    end
    thr.kill if thr.alive?
    @thr.delete(r)
    puts("OK\r\n")
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
      puts("D #{d[0 ... len]}\r\n")
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
