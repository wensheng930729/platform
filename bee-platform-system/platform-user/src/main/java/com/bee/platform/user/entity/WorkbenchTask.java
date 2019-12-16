package com.bee.platform.user.entity;

import com.baomidou.mybatisplus.activerecord.Model;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.annotations.TableName;
import com.baomidou.mybatisplus.enums.IdType;
import lombok.*;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;

/**
 * <p>
 * 工作台任务表
 * </p>
 *
 * @author hongchuan.he123
 * @since 2019-04-26
 */
@Data
@ToString
@Accessors(chain = true)
@NoArgsConstructor
@TableName("workbench_task")
public class WorkbenchTask extends Model<WorkbenchTask> {

    private static final long serialVersionUID = 1L;

    /**
     * ID
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Long id;
    /**
     * 任务编号
     */
    private String taskId;
    /**
     * 任务类型 1线上蜂贸、2蜂创物联、3领蜂供应链、4集蜂联运、5金蜜ERP
     */
    private Integer taskType;
    /**
     * 待办内容
     */
    private String backlogContent;
    /**
     * 任务状态 0待处理、1已处理
     */
    private Integer taskStatu;
    /**
     * 子系统处理地址
     */
    private String dealUrl;
    /**
     * 数据状态0删除1正常
     */
    private Integer status;
    /**
     * 创建人id
     */
    private Long createId;
    /**
     * 创建人
     */
    private String creator;
    /**
     * 创建时间
     */
    private Date createTime;
    /**
     * 修改人id
     */
    private Long modifyId;
    /**
     * 修改人名称
     */
    private String modifier;
    /**
     * 修改时间
     */
    private Date modifyTime;
    /**
     * 其他信息
     */
    private String remark;

    @Override
    protected Serializable pkVal() {
        return this.id;
    }

    @Override
    public String toString() {
        return "WorkbenchTask{" +
        ", id=" + id +
        ", taskId=" + taskId +
        ", taskType=" + taskType +
        ", backlogContent=" + backlogContent +
        ", taskStatu=" + taskStatu +
        ", dealUrl=" + dealUrl +
        ", status=" + status +
        ", createId=" + createId +
        ", creator=" + creator +
        ", createTime=" + createTime +
        ", modifyId=" + modifyId +
        ", modifier=" + modifier +
        ", modifyTime=" + modifyTime +
        ", remark=" + remark +
        "}";
    }
}
