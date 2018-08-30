
const Fly = require('./fly');
const Web = require('./web');

// silence warning caused by starting many puppeteer
process.setMaxListeners(Infinity);

class Suite {
  constructor() {
    this.url = process.env.ATC_URL || 'http://localhost:8080';
    this.username = process.env.ATC_USERNAME || 'test';
    this.password = process.env.ATC_PASSWORD || 'test';

    this.fly = new Fly(this.url, this.username, this.password);
    this.web = new Web(this.url, this.username, this.password);
    this.flys = []
  }

  async start(t) {
    await this.fly.setup();

    await this.web.expensiveInitThing();
    this.teamName = await this.fly.newTeam();

    t.log("team:", this.teamName);

    await this.fly.loginAs(this.teamName);
    await this.web.login(t);

    this.succeeded = false;
  }

  passed(t) {
    this.succeeded = true;
  }

  async finish(t) {
    await this.fly.cleanup();
    for (let index = 0; index < this.flys.length; index++) {
      await this.flys[index].cleanup();
    }

    if (this.web.page && !this.succeeded) {
      await this.web.page.screenshot({path: 'failure.png'});
    }

    if (this.web.browser) {
      await this.web.browser.close();
    }
  }

  async newFly(username, password) {
    let userFly = new Fly(this.url, username, password);
    await userFly.setup();
    this.flys = this.flys.push(userFly);
    return userFly;
  }
}

module.exports = Suite;
