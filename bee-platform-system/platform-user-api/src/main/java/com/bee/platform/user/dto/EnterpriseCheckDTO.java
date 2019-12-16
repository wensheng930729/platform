 package com.bee.platform.user.dto;

 import com.bee.platform.common.entity.RegionInfo;
 import io.swagger.annotations.ApiModel;
 import io.swagger.annotations.ApiModelProperty;
 import lombok.Data;
 import lombok.NoArgsConstructor;
 import lombok.experimental.Accessors;

 import java.io.Serializable;
 import java.sql.Timestamp;

@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "企业审核信息")
public class EnterpriseCheckDTO implements Serializable{

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("审核表中待审核企业的id")
    private int id;

    @ApiModelProperty("审核表中待审核企业的名称")
    private String name;

    @ApiModelProperty("审核表中待审核企业的联系方式(座机)")
    private String contact;

    @ApiModelProperty("审核表中待审核企业的执照号码")
    private String licence;

    @ApiModelProperty("审核表中待审核企业的执照附件")
    private String enclosure;

    @ApiModelProperty("审核表中待审核企业的地址")
    private String address;

    @ApiModelProperty("审核表中待审核企业的管理员")
    private String admin;

    @ApiModelProperty("审核表中待审核企业的状态(0: 拒绝，1: 通过, 2: 更新, 3: 新建)")
    private Integer type;

    @ApiModelProperty("审核表中待审核企业的审核员")
    private Integer checkId;

    @ApiModelProperty("企业在企业表中的真实id")
    private Integer realId;

    @ApiModelProperty("审核表中待审核企业的创建日期")
    private Timestamp createAt;

    @ApiModelProperty("审核表中待审核企业的更新日期")
    private Timestamp updateAt;

    @ApiModelProperty("审核表中待审核企业的县级地区id")
    private  String  regionid;

    @ApiModelProperty("审核表中待审核企业的指定联系人")
    private  String  linkman;

    @ApiModelProperty("审核表中待审核企业的详细地址")
    private  String  street;

    @ApiModelProperty("审核表中待审核企业的开户许可证")
    private  String  permit;

    @ApiModelProperty("审核表中待审核企业的县级地区id")
    private  String  certificate;

    @ApiModelProperty("企业类型 1企业 2物流商")
    private int enterprisesType;

    @ApiModelProperty("企业所属行业")
    private String industry;

    @ApiModelProperty("审核失败原因")
    private String failureReason;

    @ApiModelProperty("获取到区级信息")
    private RegionInfo county;

    @ApiModelProperty("获取到市级信息")
    private RegionInfo city;

    @ApiModelProperty("获取到省级信息")
    private RegionInfo province;

}
