package com.bee.platform.user.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotEmpty;
import java.io.Serializable;

/**
 * @description: 企业信息
 * @author: qhwang
 * @create: 2019-03-18 13:40
 **/
@Getter
@Setter
@NoArgsConstructor
@ToString
@Accessors(chain = true)
@ApiModel("企业信息")
public class EnterpriseInfoRQ implements Serializable {

    /**
	 * 
	 */
	private static final long serialVersionUID = -9219380468538622980L;

	@ApiModelProperty("ID")
    private Integer id;

    @ApiModelProperty("企业名称")
    @NotEmpty(message = "企业名称不能为空！")
    private String name;

    @ApiModelProperty("联系电话")
    @NotEmpty(message = "联系电话不能为空！")
    private String contact;

    @ApiModelProperty("联系人")
    @NotEmpty(message = "联系人不能为空！")
    private String linkman;

    @ApiModelProperty("详细地址")
    @NotEmpty(message = "详细地址不能为空！")
    private String address;

    @ApiModelProperty("营业执照")
    private String enclosure;

    @ApiModelProperty("开户许可证")
    private String permit;

    @ApiModelProperty("企业授权书")
    private String certificate;

}
