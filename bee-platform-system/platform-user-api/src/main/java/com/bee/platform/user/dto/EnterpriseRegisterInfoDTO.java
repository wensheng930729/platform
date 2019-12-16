package com.bee.platform.user.dto;

import com.bee.platform.user.rq.EnterprisesAttachmentRQ;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.List;

/**
 * @ClassName EnterpriseRegisterInfoDTO
 * @Description 企业注册信息返回数据
 * @author cheng.ke
 * @Date 2019/3/18$ 15:46$
 * @version 1.0.0
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("企业注册信息")
public class EnterpriseRegisterInfoDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("ID")
    private Integer id;

    @ApiModelProperty("企业名称")
    private String name;

    @ApiModelProperty("联系电话")
    private String contact;

    @ApiModelProperty("联系人")
    private String linkman;

    @ApiModelProperty("区域Id")
    private String regionid;

    @ApiModelProperty("营业执照")
    private List<EnterprisesAttachmentRQ> enclosuresList;

    @ApiModelProperty("营业许可证")
    private List<EnterprisesAttachmentRQ> permitsList;

    @ApiModelProperty("企业授权书")
    private List<EnterprisesAttachmentRQ> certificatesList;

    @ApiModelProperty("指定手机号")
    private String admin;

    @ApiModelProperty("公司详细街道")
    private String street;

    @ApiModelProperty("类型 0: 未通过，1: 已通过, 2: 未审核 (只有未通过的可进行重新编辑提交)")
    private Integer type;

    @ApiModelProperty("所属行业")
    private String industry;

    @ApiModelProperty("审核失败原因")
    private String failureReason;


}
