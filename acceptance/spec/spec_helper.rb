require 'capybara/rspec'
require 'capybara-screenshot/rspec'
require 'selenium/webdriver'
require 'stringio'
require 'fly'
require 'dash'
require 'tmpdir'
require 'rspec/retry'
require 'net/http'
require 'net/https'
require 'uri'

ATC_URL = ENV.fetch('ATC_URL', 'http://localhost:8080').freeze

ATC_USERNAME = ENV.fetch('ATC_USERNAME', "test#{ENV['TEST_ENV_NUMBER']}").freeze
ATC_PASSWORD = ENV.fetch('ATC_PASSWORD', "test#{ENV['TEST_ENV_NUMBER']}").freeze

RSpec.configure do |config|
  include Fly
  config.include Dash

  config.verbose_retry = true
  config.display_try_failure_messages = true

  config.around(:each) do |ex|
    ex.run_with_retry retry: 3
  end

  config.after(:each) do
    fly_login 'main'
    fly_with_input("destroy-team -n #{team_name}", team_name)
    fly('logout')
  end
end

Capybara.register_driver :chrome do |app|
  Capybara::Selenium::Driver.new(app, browser: :chrome)
end

Capybara.register_driver :headless_chrome do |app|
  capabilities = Selenium::WebDriver::Remote::Capabilities.chrome(
    chromeOptions: { args: %w[
      headless
      disable-gpu
      no-sandbox
      window-size=2560,1440
    ] }
  )

  Capybara::Selenium::Driver.new app,
                                 browser: :chrome,
                                 desired_capabilities: capabilities
end

Capybara.default_driver = :headless_chrome
Capybara.javascript_driver = :headless_chrome

Capybara.save_path = '/tmp'

Capybara.default_max_wait_time = 10

Capybara::Screenshot.register_driver(:headless_chrome) do |driver, path|
  driver.browser.save_screenshot(path)
end

Capybara::Screenshot.after_save_screenshot do |path|
  puts `curl -F file=@#{path} https://imagebin.ca/upload.php --progress-bar`
end
