package com.bee.platform.user.entity;

import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.annotations.TableName;
import com.baomidou.mybatisplus.enums.IdType;
import lombok.Data;
import lombok.experimental.Accessors;

/**
 * @ClassName: HelperRef
 * @Description: 帮助首页大类与文章关联表
 * @Author: fei.sun
 * @Date: 2019/5/13 10:26
 * @Version: 1.0
 */

@Data
@Accessors(chain=true)
@TableName("m_helper_relation")
public class HelperRef {

    @TableId(value = "id",type = IdType.AUTO)
    private Integer id;
    private Integer pId;
    private Integer sId;
    private Integer cId;
    private Integer weights;
}
