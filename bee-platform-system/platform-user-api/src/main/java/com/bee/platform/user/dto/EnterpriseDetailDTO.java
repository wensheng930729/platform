package com.bee.platform.user.dto;

import com.bee.platform.common.dto.IndustryDTO;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @description: 企业详细
 * @author: junyang.li
 * @create: 2019-03-22 09:47
 **/
@NoArgsConstructor
@Getter
@Setter
@ToString
@Accessors(chain = true)
@ApiModel("企业详细")
public class EnterpriseDetailDTO implements Serializable {

    private static final long serialVersionUID = -9030710828035034854L;
    @ApiModelProperty("企业id")

    private Integer id;
    @ApiModelProperty("企业名称")

    private String name;
    @ApiModelProperty("企业头像")

    private String head;
    @ApiModelProperty("企业联系方式(座机号码)")

    private String contact;
    @ApiModelProperty("企业执照号码")

    private String licence;
    @ApiModelProperty("企业执照附件")

    private String enclosure;
    @ApiModelProperty("详细地址")

    private String address;

    @ApiModelProperty("企业类型 1企业 2物流商")
    private Integer type;

    @ApiModelProperty("企业所属行业")
    private IndustryDTO industry;
}
