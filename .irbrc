#!/usr/bin/ruby

require 'irb/completion'
require 'irb/ext/save-history'

IRB.conf[:EVAL_HISTORY] = 1000
IRB.conf[:SAVE_HISTORY] = 1000
IRB.conf[:HISTORY_FILE] = "#{ENV['HOME']}/.irb_history"
#IRB.conf[:PROMPT_MODE] = :SIMPLE

require 'pp'

#load extra stuff for rails sessions
railsrc_path = File.expand_path('~/.irbc_rails')
if (ENV['RAILS_ENV'] || defined? Rails) && File.exist?(railsrc_path)
  begin
    load railsrc_path
  rescue Exception
    warn "Could not load #{railsrc_path} because of #{$!.message}"
  end
end

class Object
  # List methods which are not in superclass
  def local_methods(obj = self)
    case self.class
    when Class
      self.public_methods.sort - Object.public_methods
    when Module
      self.public_methods.sort - Module.public_methods
    else
      self.public_methods.sort - Object.new.public_methods
    end
  end

  # print documentation
  #
  # ri 'Array#pop'
  # Array.ri
  # Arrai.ri :pop
  # arr.ri :pop
  def ri(method = nil)
    unless method && method =~ /^[A-Z]/ # if class isn't specified
      klass = self.kind_of?(Class) ? name : self.class.name
      method = [klass, method].compact.join('#')
    end
    system 'ri', method.to_s
  end
end

def copy(str)
  IO.popen('pbcopy', 'w') {|f| f << str.to_s }
end

def copy_history
  history = Readline::HISTORY.entries
  index = history.rindex("exit") || -1
  content = history[(index + 1)..-2].join("\n")
  puts content
  copy content
end

def paste
  `pbpaste`
end
