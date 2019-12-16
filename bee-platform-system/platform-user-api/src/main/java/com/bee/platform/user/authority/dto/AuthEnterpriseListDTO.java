package com.bee.platform.user.authority.dto;

import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.util.Date;
import java.util.List;

/**
 * @author liang.li
 * @ClassName AuthEnterpriseAddRQ
 * @Description AuthEnterpriseListDTO企业列表
 * @Date 2019-5-20
 */
@Getter
@Setter
@ToString
@Accessors(chain = true)
@NoArgsConstructor
public class AuthEnterpriseListDTO {


    @ApiModelProperty("企业id")
    private Integer id;

    @ApiModelProperty("公司全称")
    private String name;

    @ApiModelProperty("上级企业id")
    private Integer pid;

    @ApiModelProperty("上级公司全称")
    private String pname;

    @ApiModelProperty("公司地址")
    private String address;

    @ApiModelProperty("管理员")
    private List<MemberDto> managers;

    @ApiModelProperty("删除状态0未删除 1已删除")
    private Integer deleted;

    @ApiModelProperty("状态")
    private Integer status;

    @ApiModelProperty("创建时间")
    private Date createTime;

    @ApiModelProperty("删除时间")
    private Date deletedTime;


}
