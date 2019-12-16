package com.bee.platform.user.entity;

import java.io.Serializable;

import java.util.Date;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.annotations.TableName;
import com.baomidou.mybatisplus.enums.IdType;
import com.baomidou.mybatisplus.activerecord.Model;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * <p>
 * 工单沟通详情表
 * </p>
 *
 * @author qhwang
 * @since 2019-04-25
 */
@Data
@ToString
@Accessors(chain = true)
@NoArgsConstructor
@TableName("work_orders_detail")
public class WorkOrdersDetail extends Model<WorkOrdersDetail> {

    private static final long serialVersionUID = -2410579620745228585L;

    /**
     * ID
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Long id;
    /**
     * 工单编号
     */
    private String workOrderNumber;
    /**
     * 回复详情
     */
    private String replyDescription;
    /**
     * 附件
     */
    private String enclosure;
    /**
     * 回复人id
     */
    private Long replyId;
    /**
     * 回复人
     */
    private String replyName;
    /**
     * 回复人头像
     */
    private String replyHead;
    /**
     * 回复时间
     */
    private Date replyTime;

    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
