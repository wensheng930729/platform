package com.bee.platform.user.dto;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

import com.bee.platform.user.rq.EnterprisesAttachmentRQ;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

@Getter
@Setter
@ToString
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel("审核企业详情返回对象")
public class EnterpriseCheckDetailDTO implements Serializable {
	
	private static final long serialVersionUID = 1L;
	
	@ApiModelProperty("EnterpriseCheckId")
	private Integer id;
    
	@ApiModelProperty("企业的状态(0----未通过|认证 1----通过|认证 2----未审核|认证 3----未通过|修改 4----通过|修改 5----未审核|修改)")
    private Integer type;

	@ApiModelProperty("审核失败原因")
    private String failureReason;

	@ApiModelProperty("企业表中的真实id")
    private Integer realId;

	@ApiModelProperty("企业的名称")
    private String name;

	@ApiModelProperty("指定联系人")
    private String linkman;

	@ApiModelProperty("公司联系方式")
    private String contact;

	@ApiModelProperty("所属行业")
    private String industry;

	@ApiModelProperty("企业地址")
    private String address;

	@ApiModelProperty("县级地区id")
    private String regionid;

	@ApiModelProperty("详细街道地址")
    private String street;

	@ApiModelProperty("营业执照")
    private List<EnterprisesAttachmentRQ> enclosuresList;

    @ApiModelProperty("营业许可证")
    private List<EnterprisesAttachmentRQ> permitsList;

    @ApiModelProperty("企业授权书")
    private List<EnterprisesAttachmentRQ> certificatesList;

	@ApiModelProperty("管理员")
	private List<EnterprisesUserDTO> enterprisesAdmins;
	
	@ApiModelProperty("普通用户")
	private List<EnterprisesUserDTO> userDTOs;
	
	@ApiModelProperty("开通的产品")
    private List<EnterprisesAppsOpenedDTO> appsDTOs;
	
	@ApiModelProperty("修改人id")
    private Integer modifyId;
	
	@ApiModelProperty("修改人名称")
    private String modifier;
	
	@ApiModelProperty("更新日期")
    private Date updateAt;
	
}
