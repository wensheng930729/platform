package com.bee.platform.user.authority.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import java.util.List;

/**
 * @author liang.li
 * @ClassName AuthEnterpriseAddRQ
 * @Description auth企业新增rq
 * @Date 2019-5-20
 */
@Getter
@Setter
@ToString
@Accessors(chain = true)
@NoArgsConstructor
@ApiModel(value = "新权限：添加企业的rq")
public class AuthEnterpriseAddRQ {

    @ApiModelProperty("上级公司")
    private Integer id;

    @ApiModelProperty("公司全称")
    @NotEmpty(message = "公司全称不能为空")
    private String name;

    @ApiModelProperty("公司简称")
    private String simpleName;

    @ApiModelProperty("公司logo")
    private List<FileRQ> logosList;

    @ApiModelProperty("公司联系方式")
    @NotEmpty(message = "公司联系方式不能为空")
    private String contact;

    @ApiModelProperty("指定联系人")
    @NotEmpty(message = "指定联系人不能为空")
    private String linkman;

    @ApiModelProperty("联系人昵称")
    private String nickname;

    @ApiModelProperty("县级地区id")
    @NotNull(message = "公司地址不能为空")
    private Integer regionid;

    @ApiModelProperty("详细地址")
    @NotEmpty(message = "详细地址不能为空")
    private String street;

    @ApiModelProperty("所属行业")
    @NotNull(message = "所属行业不能为空")
    private Integer industry;

    @ApiModelProperty("营业执照")
    @NotNull(message = "营业执照不能为空")
    private List<FileRQ> enclosuresList;

    @ApiModelProperty("开户许可证")
    private List<FileRQ> permitsList;

    @ApiModelProperty("企业认证授权书")
    @NotNull(message = "企业认证授权书不能为空")
    private List<FileRQ> certificatesList;

}
