package com.bee.platform.user.authority.dto;

import com.bee.platform.user.authority.rq.FileRQ;
import io.swagger.annotations.ApiModel;
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
 * @ClassName AuthEnterpriseDetailDTO
 * @Description auth企业历史详情DTO
 * @Date 2019-5-28
 */
@Getter
@Setter
@ToString
@Accessors(chain = true)
@NoArgsConstructor
@ApiModel("新权限：企业历史详情DTO")
public class AuthEnterpriseHistoryDTO {

    @ApiModelProperty("公司id")
    private Integer id;

    @ApiModelProperty("公司全称")
    private String name;

    @ApiModelProperty("详细地址")
    private String address;

    @ApiModelProperty("公司联系方式")
    private String contact;

    @ApiModelProperty("指定联系人")
    private String linkman;

    @ApiModelProperty("所属行业")
    private String industry;

    @ApiModelProperty("更新时间")
    private Date updateTime;

    @ApiModelProperty("部门")
    private List<DepartmentDTO> departments;

    @ApiModelProperty("职位")
    private List<ZPostDTO> posts ;

    @ApiModelProperty("成员")
    private List<MemberDto> members ;

    @ApiModelProperty("营业执照")
    private List<FileRQ> enclosuresList;

    @ApiModelProperty("开户许可证")
    private List<FileRQ> permitsList;

    @ApiModelProperty("企业认证授权书")
    private List<FileRQ> certificatesList;

}
