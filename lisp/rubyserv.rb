# A simple IPC server executing Ruby programs.

require 'thread'

class RubyServ
  def initialize
    @buf = ''
    @que = Queue.new
    @thr = Hash.new
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
    name, code = r.split(/\s+/, 2)
    if @thr.include?(name) && @thr[name].alive?
      puts("ERR 105 Parameter error: \"#{name}\" is already in use\r\n")
      return
    end
    code = deq_data unless code
    puts("OK\r\n")
    @thr[name] = Thread.current
    Thread.current[:rubyserv_name] = name
    begin
      Thread.current[:rubyserv_error] = false
      Thread.current[:rubyserv_response] = eval(code)
    rescue Exception => e
      Thread.current[:rubyserv_error] = true
      Thread.current[:rubyserv_response] = e.to_s.sub(/\A.*?\n/m, '')
    end
    puts("# exit #{name}\r\n")
  end

  def dispatch_poll(c, r)
    thr = @thr[r]
    if !thr
      puts("ERR 105 Parameter error: no such name \"#{r}\"\r\n")
    elsif thr.alive?
      puts("S program running\r\n")
      puts("OK\r\n")
    else
      @thr.delete(r)
      if thr[:rubyserv_error]
        puts("S program exited\r\n")
      else
        puts("S program finished\r\n")
      end
      if d = thr[:rubyserv_response]
        send_data(d.to_s)
      end
      puts("OK\r\n")
    end
  end

  def escape(s)
    s.gsub(/[%\r\n]/) {|m| '%%%02X' % m[0]}
  end

  def unescape(s)
    s.gsub(/%([0-9A-Z][0-9A-Z])/, ['\1'].pack('H*'))
  end

  def output(s)
    puts("# output #{Thread.current[:rubyserv_name]} #{s}\r\n")
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
  serv = RubyServ.new
  while gets
    serv.dispatch($_)
  end
end
