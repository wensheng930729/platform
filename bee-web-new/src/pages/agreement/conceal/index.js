import React, { Component } from 'react'
import style from './indes.less'
import HeaderNav from '@/components/headerNav'

export default class index extends Component {
  render() {
    //协议页面
    const navMsg = [
      {
        key: 'home',
        navTitle: '首页',
        link: '/'
      },
      {
        key: 'supplyChain',
        navTitle: '领蜂供应链',
        link: '/supplyChain'
      },
      {
        key: 'ifms',
        navTitle: '蜂创物联',
        link: '/ifms'
      },
      {
        key: 'transport',
        navTitle: '集蜂联运',
        link: '/logistics'
      },
      {
        key: 'trade',
        navTitle: '线上蜂贸',
        link: ''
      },
      {
        key: 'application',
        navTitle: '金蜜应用',
        link: ''
      },
      {
        key: 'news',
        navTitle: '新闻中心',
        link: ''
      }
    ]
    return (
      <div className="conceal">
        {/* <HeaderNav
          navMsg={navMsg}
          pathName=""
          color="rgba(51,51,51,1)"
          imgMsg={{
            path: "agreement",
            imgName: "1"
          }}
        ></HeaderNav> */}
        <h1>金蜜用户隐私声明</h1>
        <p>
          金蜜工业云（以下简称金蜜）非常重视对您的个人隐私保护，有时候我们需要某些信息才能为您提供您请求的服务，
          本隐私声明解释了这些情况下的数据收集和使用情况。本隐私声明适用于金蜜的所有相关服务，
          <span className="bold">
            随着服务范围的扩大，
            隐私声明的内容可由金蜜随时更新，且毋须另行通知。更新后的隐私声明一旦在网页上公布即有效代替原来的隐私声明。
          </span>
        </p>
        <h2>一、我们收集哪些信息</h2>
        <p>
          目前金蜜只对如下数据进行收集：
          <br />
          <br />
          当前网站的网址；
          <br />
          本次验证时所处的浏览器平台；
          <br />
          本次拖动时用户的IP及验证时间。
          <br />
          金蜜不会对网站的隐私信息有任何侵犯。当我们需要能识别您的个人信息或者可以与您联系的信息时，我们会征求您的同意。
          通常，在您注册金蜜工业云或申请开通新的功能时，我们可能收集这些信息：姓名，Email地址，电话号码等。
        </p>
        <h2>二、关于您的个人信息</h2>
        <p>
          金蜜严格保护您个人信息的安全。我们使用各种安全技术和程序来保护您的个人信息不被未经授权的访问、使用或泄露。
          <br />
          金蜜会在法律要求或符合金蜜相关服务条款、软件许可使用协议约定的情况下透露您的个人信息，或者有充分理由相信必须这样做才能：
          <br />
          (a) 满足法律或行政法规的明文规定，或者符合极验适用的法律程序；
          <br />
          (b) 符合金蜜相关服务条款、软件许可使用协议的约定；
          <br />
          (c) 保护金蜜的权利或财产
          <br />
          (d) 在紧急情况下保护金蜜员工、产品或服务的用户或大众的个人安全。
          我们所收集的信息将用于修改和改进我们的服务、产品；金蜜不会未经您的允许将这些信息与第三方共享，本声明已经列出的上述情况除外。
        </p>
        <h2>三、关于免责说明</h2>
        <p className="bold">
          就下列相关事宜的发生，金蜜不承担任何法律责任：
          <br />
          * 由于您将用户密码告知他人或与他人共享注册帐户，由此导致的任何个人信息的泄露，或其他非因金蜜原因导致的个人信息的泄露；
          <br />
          * 金蜜根据法律规定或政府相关政策要求提供您的个人信息；
          <br />
          * 任何第三方根据金蜜各服务条款及声明中所列明的情况使用您的个人信息，由此所产生的纠纷；
          <br />
          * 任何由于黑客攻击、电脑病毒侵入或政府管制而造成的暂时性网站关闭；
          <br />
          * 因不可抗力导致的任何后果；
          <br />
          * 金蜜在各服务条款及声明中列明的使用方式或免责情形。
        </p>
      </div>
    )
  }
}
