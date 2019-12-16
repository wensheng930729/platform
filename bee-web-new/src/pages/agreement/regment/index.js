import React, { Component } from 'react'
import style from './index.less'
import HeaderNav from '@/components/headerNav'

export default class index extends Component {
  render() {
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
        navTitle: '金蜜ERP',
        link: '/erp'
      },
      {
        key: 'news',
        navTitle: '新闻中心',
        link: '/news'
      }
    ]
    return (
      <div className="regment">
        <h1>金蜜工业云用户注册协议</h1>
        <p>
          《金蜜工业云用户注册协议》本协议是您与金蜜工业云所有者四川金蜜信息技术有限公司（https://www.beesrv.com）之间就金蜜工业云服务等相关事宜所订立的契约，请您仔细阅读本注册协议，特别是免除和限制责任的条款、争议解决条款，如对协议有疑问，可联系金蜜工业云[beesrv@foxmail.com，028-85988267]。
          除非用户接受本协议，否则用户应立即停止注册及使用行为。您点击"阅读并同意"按钮后，即表示您已充分阅读、理解本协议并同意接受本协议全部内容，成为金蜜工业云平台的用户，并自愿遵守本协议。若我们对本协议进行修改，将通过https://www.beesrv.com网站公告的方式予以公布,您若不同意应停止使用金蜜工业云的服务，若继续使用，则视为您愿意遵守更新后的协议。
        </p>
        <br />
        <h2>第1条 服务条款的确认和接纳</h2>
        <p>
          1.1金蜜工业云的各项在线服务的所有权和运作权归金蜜工业云所有。您同意本协议所有条款并完成注册程序，才能成为金蜜工业云的正式用户。
          <br />
          1.2注册成功后，即视为您确认自己具有享受在线浏览产品及享受服务等相应的权利能力和行为能力，能够独立承担法律责任。
          <br />
          1.3金蜜工业云保留法律允许的范围内独自决定拒绝服务、关闭用户账户、清除或编辑内容或取消订单的权利。任何经金蜜工业云确认已经违反相关法律法规或本协议或金蜜工业云使用规则某一项或多项规定的用户，金蜜工业云有权决定是否给予暂停使用或终止使用的处理，且由此造成的一切后果由用户自行承担。
          <br />
          1.4金蜜工业云上的“法律声明”为本协议不可分割的一部分，与本协议具有同等法律效力。如您使用金蜜工业云的服务，即视为同意法律声明的内容。
        </p>
        <h2>第2条 服务内容</h2>
        <p>
          金蜜工业云通过互联网依法为您提供大宗产品、物流、供应链等服务，您在完全同意本协议及金蜜工业云规定的情况下，方有权使用金蜜工业云的相关服务。
        </p>
        <h2>第3条 账户注册、使用、管理</h2>
        <p>
          3.1您应本着诚信向本站提供注册资料，保证其提供的注册资料真实、准确、完整、合法有效，注册资料如有变动的，应及时更新其注册资料。否则，您需承担因此引起的相应责任及后果，并且金蜜工业云保留终止您使用金蜜工业云各项服务的权利。
          <br />
          3.1.1 企业账号
          <br />
          （1）为享有包括采购等更多的服务和平台操作功能，您可以申请企业账号。您应按照金蜜工业云企业账号认证要求提供授权书、企业营业执照，以及其他证件且经金蜜工业云审核通过后，您即为金蜜工业云认证后的企业账号。
          <br />
          （2）您完成企业账号认证后，授权书授权的手机号即自动成为企业账号管理员账号，管理员账号可审核关联企业申请的子账号，该子账号的权限以金蜜工业云平台开通的权限功能以及授权书为准。
          <br />
          （3）您在成功完成企业账号认证后，您可以通过金蜜工业云平台在线平台、发布资源信息、发布求购信息、下载资源单等企业服务。
          <br />
          3.1.2 子账号设立
          <br />
          （1）您完成企业账号认证后，在企业账号管理员的授权下，您可以在企业账号下开通相应数量的子账号，该子账号的数量、权限最终以金蜜工业云平台开通的权限功能以及授权书为准。
          <br />
          （2）子账号开通后，有独立的管理员手机号和密码，子账号可以代表您通过金蜜工业云平台上在线交易、发布资源信息、发布求购信息、下载资源单等企业会员服务。
          <br />
          （3）为保护您的信息安全，子账号无权查看企业账号及其他子账号的订单信息。
          <br />
          （4）子账号所创建的所有订单信息及操作行为，最终均统一归口为企业账号登记用户，所产生的权利义务均由其享有和承担。
          <br />
          3.2
          用户对通过其账户所进行的操作依法享有权利和承担责任，并且账号仅限于用户自身进行使用，不得给予任何第三方使用且不能以任何方式转让、赠与或继承，否则由此造成的损失由用户自行承担，且金蜜工业云保留暂停或终止服务的权利。
          <br />
          3.3您应谨慎、妥善的保存、使用密码，且勿将密码泄露于他人。否则，由于您的疏忽而导致账号信息泄露等后果，由您自行承担。
          您遗忘或丢失在金蜜工业云注册的密码时，可与金蜜工业云客服人员取得联系，在提供相关证明资料并经金蜜工业云审核确认后，可找回密码。
          <br />
          3.4
          您同意，金蜜工业云有权使用您的注册信息、密码等信息，登陆进入您的注册账户，进行证据保全，包括但不限于公证、见证等。
          <br />
          3.5
          保护用户信息是金蜜工业云的一项原则，涉及用户真实姓名/名称、通信地址、联系方式、营业执照等隐私信息的，金蜜工业云将予以严格保密，除非得到您的授权或法律另有规定，不会向外界披露用户隐私信息。
        </p>
        <h2>第4条 用户使用规范</h2>
        <p>
          用户在使用金蜜工业云服务过程中除应遵守本协议及金蜜工业云服务规则外，还应遵守如下义务：
          <br />
          <br />
          4.1在使用金蜜工业云服务过程中实施的所有行为均须遵守国家法律、法规，不得违背社会公共利益或公共道德，不得损害他人的合法权益，不违反本协议及相关规则。如果违反前述承诺，产生任何法律后果由用户自行承担责任。
          <br />
          4.2在交易过程中，遵守诚实信用原则，不采取不正当竞争行为，不扰乱金蜜工业云服务的正常秩序，不得利用金蜜工业云进行非法牟利活动。
          <br />
          4.3除金蜜工业云同意外，不得对金蜜工业云上的任何数据进行商业性利用，包括但不限于以复制、传播等任何方式使用金蜜工业云网站上展示的信息。
          <br />
          4.4不使用任何装置、软件或例行程序干预或试图干预金蜜工业云的正常运作或正在进行的任何交易、活动。
          若用户未遵守以上规定的，金蜜工业云有权作出独立判断并采取暂停或关闭用户帐号等措施。用户须对自己在网上的言论和行为承担法律责任。
        </p>
        <h2>第5条 商品信息</h2>
        <p>
          金蜜工业云上的货物价格、数量、是否有货等商品信息将根据市场行情及销售情况随时发生变动，金蜜工业云不作特别通知。由于网站货物信息数量庞大，虽然金蜜工业云会尽最大努力保证您所浏览本站信息的准确性，但由于网络及电脑终端兼容性等客观原因存在，网站网页显示的信息可能会有一定的滞后性或差错，对此情形请您知悉并理解；金蜜工业云欢迎纠错。
        </p>
        <h2>第6条 责任限制及不承诺担保</h2>
        <p>
          除非另有明确的书面说明,金蜜工业云网站上展示的全部信息、资料、货物和服务，均是在"按现状"和"按现有"的基础上提供的。
          除非另有明确的书面说明,金蜜工业云不对网站上的信息、资料、货物和服务作任何形式的、明示或默示的声明或担保（根据中华人民共和国法律另有规定的以外）。
          如因不可抗力或其它无法控制的原因导致网上交易无法完成或丢失有关的信息、记录等，金蜜工业云会合理地尽力协助处理善后事宜。
        </p>
        <h2>第7条 协议变更</h2>
        <p>
          7.1根据国家法律法规变化及网站运营需要，金蜜工业云有权对本协议条款不时地进行修改，修改后的协议一旦被公布在网站上即生效，并代替原来的协议。如用户不同意更新后的协议，可以且应立即停止使用金蜜工业云的服务；如用户继续使用，即视为同意更新后的协议。
          <br />
          7.2本协议任一条款被视为废止、无效或不可执行，该条应视为可分的且并不影响本协议其余条款的有效性及可执行性。
          <br />
          7.3用户与金蜜工业云基于交易合作签署的其他书面协议与本协议不一致的，以双方书面签署的协议为准。
        </p>
        <h2>第8条 法律适用和争议管辖</h2>
        <p>
          本协议的订立、执行和解释及争议的解决均应适用在中华人民共和国大陆地区适用之有效法律（但不包括其冲突法规则）。
          如缔约方就本协议内容或其执行发生任何争议，双方应尽力友好协商解决；协商不成时，任何一方均可向有金蜜工业云注册地（成都市高新区）人民法院提起诉讼。
        </p>
        <h2>第9条 其他</h2>
        <p>
          金蜜工业云尊重用户合法权利，本协议及金蜜工业云上发布的各类规则、声明等其他内容，均是为了更好的、更加便利的为用户提供服务。金蜜工业云欢迎用户和社会各界提出意见和建议，对此金蜜工业云将虚心接受并适时修改本协议及各类规则。
        </p>
      </div>
    )
  }
}