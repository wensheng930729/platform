package com.bee.platform.user.authority.dto;

import com.bee.platform.user.authority.rq.FileRQ;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.util.List;

/**
 * @author liang.li
 * @ClassName AuthEnterpriseFeignDetailDTO
 * @Description 查询企业详情DTO-feign使用
 * @Date 2019-8-2
 */
@Getter
@Setter
@ToString
@Accessors(chain = true)
@NoArgsConstructor
@ApiModel("查询企业详情DTO-feign使用")
public class AuthEnterpriseFeignDetailDTO {

    @ApiModelProperty("公司id")
    private Integer id;

    @ApiModelProperty("公司全称")
    private String name;

    @ApiModelProperty("公司简称")
    private String simpleName;

    @ApiModelProperty("上级公司id")
    private Integer pid;

    @ApiModelProperty("企业类型 1企业 2物流商(兼容旧版本)")
    private Integer type;

    @ApiModelProperty("企业管理员")
    private String admin;

    @ApiModelProperty("县级地区id")
    private Integer regionid;

    @ApiModelProperty("详细地址")
    private String street;

    @ApiModelProperty("地址")
    private String address;

    @ApiModelProperty("公司联系方式")
    private String contact;

    @ApiModelProperty("指定联系人")
    private String linkman;

    @ApiModelProperty("所属行业")
    private Integer industry;


}
