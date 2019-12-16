package com.bee.platform.user.dto;

import com.bee.platform.user.authority.dto.AuthCommonFileDTO;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;
import lombok.experimental.Accessors;
import java.io.Serializable;
import java.util.List;

@Data
@ToString
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel("企业管理 企业修改返回参数")
public class EnterpriseWithAttacheDTO implements Serializable {


    private static final long serialVersionUID = 3675965448291843778L;

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
    private List<AuthCommonFileDTO> enclosuresList;

    @ApiModelProperty("营业许可证")
    private List<AuthCommonFileDTO> permitsList;

    @ApiModelProperty("企业授权书")
    private List<AuthCommonFileDTO> certificatesList;

    @ApiModelProperty("公司logo")
    private List<AuthCommonFileDTO> logosList;

    @ApiModelProperty("公司详细街道")
    private String street;

    @ApiModelProperty("所属行业")
    private String industry;

    @ApiModelProperty("详细地址")
    private String address;

}
