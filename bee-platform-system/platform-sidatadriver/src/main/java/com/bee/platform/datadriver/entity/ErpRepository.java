package com.bee.platform.datadriver.entity;

import com.baomidou.mybatisplus.activerecord.Model;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.enums.IdType;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;

/**
 * <p>
 * 仓库档案
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
@Getter
@Setter
@Accessors(chain = true)
public class ErpRepository extends Model<ErpRepository> {

    private static final long serialVersionUID = 1L;

    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 名称
     */
    private String name;
    /**
     * 状态
     */
    private Integer status;
    /**
     * 所属企业id
     */
    private Integer orgId;
    /**
     * 删除状态
     */
    private Integer deleted;
    /**
     * 仓库类型--码表中
     */
    private String type;
    /**
     * 创建人id
     */
    private Integer createrId;
    /**
     * 创建时间
     */
    private Date createTime;

    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
