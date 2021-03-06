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
      <div className="wrap">
        <div className="service">
          {/* <HeaderNav
          navMsg={navMsg}
          pathName=""
          color="rgba(51,51,51,1)"
          imgMsg={{
            path: "agreement",
            imgName: "1"
          }}
        ></HeaderNav> */}
          <h1>金蜜用户使用协议</h1>
          <h2>一、特别提示</h2>
          <p>
            金蜜用户使用协议（下简称本协议）系由用户与金蜜工业云之间就金蜜工业云提供的平台服务所订立的权利义务规范，同时本协议亦适用于任何有关金蜜工业云服务的后期的更新或升级。
            本协议是用户接受金蜜工业云所提供的平台服务时所适用的通用条款，用户应当阅读并遵守本协议、使用说明及相关附件规定。
            用户选择接受服务时，应务必审慎阅读、充分理解各条款内容。
            <span className="bold">
              除非用户已阅读并接受本协议所有条款，否则无权登录、使用金蜜工业云提供的平台服务。
              用户的注册、登录、使用等行为均视为已阅读并同意本协议约束。
            </span>
          </p>
          <h2>二、 协议范围</h2>
          <p>
            本协议内容同时包括金蜜工业云可能不断发布或提示用户的关于平台服务的相关协议、使用规则、风险告知、服务公告等内容。
            上述内容一经正式发布，即视为本协议的组成部分，用户选择接受平台服务时应予以遵守。
          </p>
          <h2>三、 服务说明</h2>
          <p>
            1.
            四川金蜜信息技术有限公司为金蜜工业云平台服务的所有者及经营者，金蜜工业云平台服务的具体内容由四川金蜜信息技术有限公司根据实际情况提供，主要为贸易、物流、供应链等服务平台。
            <br />
            2．金蜜工业云提供平台服务时，可能会针对部分服务向用户收取一定的费用。在此情况下，金蜜工业云将会在相关页面作出提示。如用户不同意支付该等费用,则可不接受相关的服务，
            <span className="bold">
              同时亦无法使用相关功能，无法享受相关服务。
            </span>
            <br />
            3.金蜜工业云平台服务为互联网在线SaaS服务，在使用该服务的过程中，可能受多种因素影响不可避免的存在某种使用风险，用户应具备软件运行或接受相应平台服务所需的条件，
            用户必须保证有权使用其运行所需操作系统。在适用法律允许的最大范围内，金蜜工业云在任何情况下不就用户因非金蜜工业云原因直接造成的使用或不能使用服务所发生的特殊的、意外的、直接的或间接的侵权或遭致损失承担赔偿责任。
          </p>
          <h2>四、 信息保护</h2>
          <p>
            1.金蜜工业云将采取合理措施保护用户的信息和数据，除法律法规规定的情形外，对用户接受金蜜工业云平台服务所产生的数据和信息严格保密，未经用户许可不向第三方披露或公开。
            但下列情形不适用本保密条款
            <br />
            1.1 非因金蜜工业云的过错造成的信息披露或公开; 
            <br />
            1.2 非因使用或接受金蜜工业云的服务被他人知悉的数据或信息； 
            <br />
            1.3 因法律法规的规定，需要向法院或其他国家有权机关的披露的行为。
            <br />
            2.
            本软件不含有任何旨在破坏用户计算机数据和获取用户隐私信息的恶意代码。
          </p>
          <h2>五、 授权范围</h2>
          <p>
            1.
            用户使用增值服务，金蜜工业云平台产品及服务的所有权及经营权仍归金蜜工业云所有。
            <br />
            2.
            权利保留：本条款及本协议其他条款未明示授权的其他一切权利仍由金蜜工业云保留，用户使用其他权利时须另外取得金蜜工业云的书面同意。金蜜工业云未行使前述任何权利，并不表示对该项权利的放弃。
          </p>
          <h2>六、禁止行为</h2>
          <p>
            1.
            使用金蜜工业云平台服务过程中，用户不得违反国家有关法律和政策等，应当维护国家利益，保护国家安全，并遵守本协议。
            <br />
            2.
            除非法律、法规允许且金蜜工业云平台服务相关协议书面许可的前提下，用户在接受金蜜工业云平台服务过程中，不得实施如下行为（包括但不限于）：
            <br />
            2.1 删除金蜜工业云平台及其他副本上一切关于著作权的信息；
            <br />
            2.2
            对金蜜工业云平台进行反向工程，如反汇编、反编译，或者以其他方式尝试发现金蜜工业云平台的源代码等；
            <br />
            2.3 修改、破坏金蜜工业云平台原状；
            <br />
            2.4
            进行任何危害网络安全的行为，包括但不限于：使用未经许可的数据或进入未经许可的服务器/帐户；未经允许进入公众网络或者他人操作系统并删除、修改、增加、复制存储信息；
            <br />
            2.5未经许可企图探查、扫描、测试金蜜工业云平台网络的弱点或其它实施破坏网络安全的行为；企图干涉、破坏金蜜工业云平台的正常运行，故意传播恶意程序或病毒以及其他破坏干扰正常网络信息服务的行为；
            伪造TCP/IP数据包名称或部分名称；
            <br />
            2.6
            未经金蜜工业云平台书面同意，擅自实施包括但不限于下列行为：使用、修改、链接，借助金蜜工业云平台发展与之有关的衍生产品、作品、服务、外挂、兼容、互联等；
            <br />
            2.7
            利用金蜜工业云平台发表、传送、传播、储存侵害他人知识产权、商业秘密权等合法权利的内容；
            <br />
            2.8 利用金蜜工业云平台批量发表、传送、传播广告信息及垃圾信息；
            <br />
            2.9
            传送、散布或以其他方式实现侵犯第三人权利的图像、相片、软件、信息或其他资料文件，包括但不限于著作权、商标法、肖像权、商业信息等，除非用户已征得相关权利人的书面许可；
            <br />
            2.10
            通过修改或伪造软件运行中的指令、数据、数据包，增加、删减、变动软件的功能或运行效果，或将用于上述用途的软件通过信息网络及其他方式向公众传播或者运营；
            <br />
            2.11
            在未经金蜜工业云书面明确授权的前提下，出售、出租、出借、散布、转移或转授权软件和服务或相关的链接或从使用软件和服务或软件和服务的条款中获利，无论以上使用是否为直接经济或金钱收益；
            <br />
            2.12
            其他以任何不合法的方式、或为任何非法目的、或以任何与本协议不一致的方式使用金蜜工业云平台和金蜜工业云提供的其他服务；
            <br />
            2.13 其他法律法规或相关国家政策禁止或未经金蜜工业云书面授权的行为。
            <br />
            3.
            用户若违反上述规定，金蜜工业云有权采取终止、完全或部分中止、限制用户使用金蜜工业云平台的功能。如因用户上述行为，导致或产生第三方主张的任何索赔、要求或损失，由用户自行承担；
            由此造成金蜜工业云损失的，金蜜工业云有权追偿。
          </p>
          <h2>七、使用规范</h2>
          <p>
            1.
            用户在接受金蜜工业云平台服务时，必须遵守中华人民共和国相关法律法规的规定，用户应同意将不会利用金蜜工业云平台产品或服务进行任何违法或不正当的行为，包括但不限于下列行为：
            <br />
            1.1
            上传、下载、张贴或以其他方式提供任何非法、有害、胁迫、骚扰、侵权、中伤、粗俗、猥亵、诽谤、淫秽、暴力、侵害他人隐私、种族歧视或其他违法的信息，包括但不限于资讯、资料、文字、照片、图形、信息或其他资料；
            <br />
            1.2 以任何方式危害未成年人；
            <br />
            1.3
            冒充任何人或机构，或以虚伪不实的方式谎称或使人误认为与任何人或任何机构有关；
            <br />
            1.4 伪造标题或以其他方式操控识别资料，使他人产生误解；
            <br />
            1.5 上传、张贴或以其他方式传送无权传送的内容；
            <br />
            1.6
            侵犯他人著作权或其他知识产权，或违反保密、雇佣或不披露协议披露他人商业秘密或保密信息；
            <br />
            1.7
            张贴、发送或以其他方式提供任何未经权利人许可或授权的电子信息、广告、促销资料等，包括但不限于大批量的商业广告和信息公告；
            <br />
            1.8
            干扰或破坏有关服务，或与有关服务连接的任何服务器或网络，或与有关服务连接之网络的任何政策、要求或规定；
            <br />
            1.9
            采集并存储涉或以其他方式侵犯其他用户的个人信息，无论该行为是否用于何种目的；
            <br />
            1.10
            故意或非故意违反任何相关的中国法律、法规、政策、规章、条例等其他具有法律效力的规范性文件。
            <br />
            2.
            用户对以其账号发生的或通过其账号发生的一切活动和事件，包括但不限于用户上传的图片以及由此产生的任何结果，自行承担全部法律责任。
          </p>
          <h2>八、违约责任</h2>
          <p>
            1.
            用户违反本协议任一条款约定，均视为违约。金蜜工业云有权选择采取终止、完全或部分中止、限制用户使用金蜜工业云平台的功能。
            <br />
            2.
            用户理解并同意，因用户违反本协议或相关服务条款的规定，导致或产生第三方主张的任何索赔、要求或损失，由用户独立承担责任。
            由此导致金蜜工业云遭受损失的，用户应当予以赔偿。
            <br />
            3.金蜜工业云拥有平台的著作权，该权利受《中华人民共和国著作权法》、《计算机软件保护条例》等中国法律、法规及相关国际公约的保护，任何未经授权而擅自修改、复制、发行、出租、传播、翻译、反向编译或反向工程本软件程序的全部或部分，均属于侵权行为，金蜜工业云保留依法追究其法律责任的权利。
          </p>
          <h2>九、协议终止</h2>
          <p>
            如果用户违反本协议项下任一条款约定的，
            <span className="bold">
              金蜜工业云有权立即停止用户的金蜜工业云平台服务，金蜜工业云无需另行通知用户
            </span>{' '}
            ；用户承担相应违反本协议及金蜜工业云服务相关协议责任后，本协议终止。本协议终止后，金蜜工业云有权将服务过程中的相关信息、资料等彻底删除。
          </p>
          <h2>十、 通知送达</h2>
          <p className="bold">
            金蜜工业云如提示某项服务收费或发布使用规则或风险告知或协议修订或系统升级等相关事项，
            将在用户使用的web界面或服务平台发布有关事项的公告，自公告发布之日起生效。届时，用户在选择接受服务时，视为已通知送达用户，金蜜工业云无需另行通知。
          </p>
          <h2>十一、协议修改</h2>
          <p>
            1．
            <span className="bold">
              金蜜工业云有权随时修订本协议的任何条款，一旦本协议的内容发生变动，金蜜工业云将会在金蜜工业云平台网站上公布修订后的协议内容。
              同时，金蜜工业云也可选择其他合理方式（比如系统通知）向用户通知修订内容。
            </span>
            <br />
            2.
            用户不同意金蜜工业云对本协议的修订内容，应停止注册、登录、使用金蜜工业云平台服务的行为，金蜜工业云亦有权终止双方的服务。
            <br />
            3.{' '}
            <span className="bold">
              用户继续接受金蜜工业云平台服务的，则视为用户同意对本协议相关条款所做的修订。
            </span>
          </p>
          <h2>十二、其他事项</h2>
          <p>
            1.
            金蜜工业云有可能针对本协议某项服务添加相关特别约定或服务协议。届时，用户接受该项服务或产品的前提为同意该项服务所附带的所有规范性文件，
            <span className="bold">
              包括但不限于本服务使用协议及专项服务协议等
            </span>{' '}
            。
            <br />
            2.{' '}
            <span className="bold">
              除本协议规定之外，未赋予本协议各方其他权利
            </span>{' '}
            。
            <br />
            3.
            如本协议中的任何条款无论因何种原因完全或部分无效或不具有约束力，本协议的其余条款仍应有效，对双方具有约束力。
            <br />
            4.{' '}
            <span className="bold">
              因本协议发生的纠纷，双方应协商解决，协商不能的，适用中华人民共和国法律，由四川金蜜信息技术有限公司住所地的人民法院管辖
            </span>{' '}
            。
            <br />
            5. <span className="bold">在法律允许的最大范围内</span>{' '}
            ，本服务协议最终解释权归金蜜工业云所有
          </p>
        </div>
      </div>
    )
  }
}
