package com.bee.platform.user.authority.rq;

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
 * @ClassName AuthEnterpriseAddRQ
 * @Description auth企业新增rq
 * @Date 2019-5-20
 */
@Getter
@Setter
@ToString
@Accessors(chain = true)
@NoArgsConstructor
@ApiModel("新权限：更新企业RQ")
public class AuthEnterpriseUpdateRQ {

    @ApiModelProperty("公司id")
    private Integer id;

    @ApiModelProperty("公司全称")
    private String name;

    @ApiModelProperty("公司简称")
    private String simpleName;

    @ApiModelProperty("公司logo")
    private List<FileRQ> logosList;

    @ApiModelProperty("上级公司")
    private Integer pid;

    @ApiModelProperty("县级地区id")
    private Integer regionid;

    @ApiModelProperty("详细地址")
    private String address;

    @ApiModelProperty("公司联系方式")
    private String contact;

    @ApiModelProperty("指定联系人")
    private String linkman;

    @ApiModelProperty("所属行业")
    private Integer industry;

    @ApiModelProperty("营业执照")
    private List<FileRQ> enclosuresList;

    @ApiModelProperty("开户许可证")
    private List<FileRQ> permitsList;

    @ApiModelProperty("企业认证授权书")
    private List<FileRQ> certificatesList;

}
