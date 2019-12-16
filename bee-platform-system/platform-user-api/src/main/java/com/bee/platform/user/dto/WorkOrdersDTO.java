package com.bee.platform.user.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.util.Date;
import java.util.List;

/**
 * @ClassName WorkOrdersDTO
 * @Description 工单信息
 * @author qhwang
 * @version 1.0.0
 * @Date 2019/4/25 14:39
 */
@Data
@ToString
@Accessors(chain = true)
@NoArgsConstructor
@ApiModel("工单信息")
public class WorkOrdersDTO {

    @ApiModelProperty("工单编号 ")
    private String workOrderNumber;

    @ApiModelProperty("工单标题")
    private String workOrderTitle;

    @ApiModelProperty("优先级1重要2一般")
    private Integer priority;

    @ApiModelProperty("工单状态 1待平台处理、2平台处理中、3待用户确认、4已关闭")
    private Integer orderStatus;

    @ApiModelProperty("问题所属产品")
    private Integer belongApp;

    @ApiModelProperty("问题所属产品名称")
    private String belongAppName;

    @ApiModelProperty("提交人所属公司")
    private Integer belongCompany;

    @ApiModelProperty("提交人所属公司名称")
    private String belongCompanyName;

    @ApiModelProperty("提交人在所属公司角色名称")
    private String roleName;

    @ApiModelProperty("问题描述")
    private String problemDescription;

    @ApiModelProperty("联系手机号码")
    private String phone;

    @ApiModelProperty("附件")
    private List<AttachmentDTO> enclosure;

    @ApiModelProperty("提交时间")
    private Date submitTime;

    @ApiModelProperty("最近受理时间")
    private Date recentAcceptTime;

    @ApiModelProperty("问题解决时间")
    private Date problemSolvingTime;

    @ApiModelProperty("受理人id")
    private Long acceptId;

    @ApiModelProperty("受理人")
    private String acceptor;

    @ApiModelProperty("创建人id")
    private Long createId;

    @ApiModelProperty("创建人")
    private String creator;

    @ApiModelProperty("创建人头像")
    private String createHead;

    @ApiModelProperty("工单沟通详情信息")
    private List<WorkOrdersDetailDTO> detailDTOS;

}
